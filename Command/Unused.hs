{- git-annex command
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command.Unused where

import qualified Data.Set as S
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.BloomFilter
import Data.BloomFilter.Easy
import Data.BloomFilter.Hash
import Control.Monad.ST

import Common.Annex
import Command
import Annex.Content
import Utility.FileMode
import Utility.TempFile
import Logs.Location
import Config
import qualified Annex
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Git.LsFiles as LsFiles
import qualified Git.LsTree as LsTree
import qualified Backend
import qualified Remote
import qualified Annex.Branch
import qualified Option
import Annex.CatFile

def :: [Command]
def = [withOptions [fromOption] $ command "unused" paramNothing seek
	"look for unused file content"]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "remote to check for unused content"

seek :: [CommandSeek]
seek = [withNothing start]

{- Finds unused content in the annex. -} 
start :: CommandStart
start = do
	from <- Annex.getField $ Option.name fromOption
	let (name, action) = case from of
		Nothing -> (".", checkUnused)
		Just "." -> (".", checkUnused)
		Just "here" -> (".", checkUnused)
		Just n -> (n, checkRemoteUnused n)
	showStart "unused" name
	next action

checkUnused :: CommandPerform
checkUnused = chain 0
	[ check "" unusedMsg $ findunused =<< Annex.getState Annex.fast
	, check "bad" staleBadMsg $ staleKeysPrune gitAnnexBadDir
	, check "tmp" staleTmpMsg $ staleKeysPrune gitAnnexTmpDir
	]
	where
		findunused True = do
			showNote "fast mode enabled; only finding stale files"
			return []
		findunused False = do
			showAction "checking for unused data"
			excludeReferenced =<< getKeysPresent
		chain _ [] = next $ return True
		chain v (a:as) = do
			v' <- a v
			chain v' as

checkRemoteUnused :: String -> CommandPerform
checkRemoteUnused name = go =<< fromJust <$> Remote.byName (Just name)
	where
		go r = do
			showAction "checking for unused data"
			_ <- check "" (remoteUnusedMsg r) (remoteunused r) 0
			next $ return True
		remoteunused r =
			excludeReferenced =<< loggedKeysFor (Remote.uuid r)

check :: FilePath -> ([(Int, Key)] -> String) -> Annex [Key] -> Int -> Annex Int
check file msg a c = do
	l <- a
	let unusedlist = number c l
	unless (null l) $ showLongNote $ msg unusedlist
	writeUnusedFile file unusedlist
	return $ c + length l

number :: Int -> [a] -> [(Int, a)]
number _ [] = []
number n (x:xs) = (n+1, x) : number (n+1) xs

writeUnusedFile :: FilePath -> [(Int, Key)] -> Annex ()
writeUnusedFile prefix l = do
	logfile <- fromRepo $ gitAnnexUnusedLog prefix
	liftIO $ viaTmp writeFile logfile $
		unlines $ map (\(n, k) -> show n ++ " " ++ show k) l

table :: [(Int, Key)] -> [String]
table l = "  NUMBER  KEY" : map cols l
	where
		cols (n,k) = "  " ++ pad 6 (show n) ++ "  " ++ show k
		pad n s = s ++ replicate (n - length s) ' '

staleTmpMsg :: [(Int, Key)] -> String
staleTmpMsg t = unlines $ 
	["Some partially transferred data exists in temporary files:"]
	++ table t ++ [dropMsg Nothing]

staleBadMsg :: [(Int, Key)] -> String
staleBadMsg t = unlines $ 
	["Some corrupted files have been preserved by fsck, just in case:"]
	++ table t ++ [dropMsg Nothing]

unusedMsg :: [(Int, Key)] -> String
unusedMsg u = unusedMsg' u
	["Some annexed data is no longer used by any files:"]
	[dropMsg Nothing]
unusedMsg' :: [(Int, Key)] -> [String] -> [String] -> String
unusedMsg' u header trailer = unlines $
	header ++
	table u ++
	["(To see where data was previously used, try: git log --stat -S'KEY')"] ++
	trailer

remoteUnusedMsg :: Remote -> [(Int, Key)] -> String
remoteUnusedMsg r u = unusedMsg' u
	["Some annexed data on " ++ name ++ " is not used by any files:"]
	[dropMsg $ Just r]
	where
		name = Remote.name r 

dropMsg :: Maybe Remote -> String
dropMsg Nothing = dropMsg' ""
dropMsg (Just r) = dropMsg' $ " --from " ++ Remote.name r
dropMsg' :: String -> String
dropMsg' s = "\nTo remove unwanted data: git-annex dropunused" ++ s ++ " NUMBER\n"

{- Finds keys in the list that are not referenced in the git repository.
 -
 - Strategy:
 -
 - * Build a bloom filter of all keys referenced by symlinks. This 
 -   is the fastest one to build and will filter out most keys.
 - * If keys remain, build a second bloom filter of keys referenced by
 -   all branches.
 - * The list is streamed through these bloom filters lazily, so both will
 -   exist at the same time. This means that twice the memory is used,
 -   but they're relatively small, so the added complexity of using a
 -   mutable bloom filter does not seem worthwhile.
 - * Generating the second bloom filter can take quite a while, since
 -   it needs enumerating all keys in all git branches. But, the common
 -   case, if the second filter is needed, is for some keys to be globally
 -   unused, and in that case, no short-circuit is possible.
 -   Short-circuiting if the first filter filters all the keys handles the
 -   other common case.
 -}
excludeReferenced :: [Key] -> Annex [Key]
excludeReferenced ks = runfilter firstlevel ks >>= runfilter secondlevel
	where
		runfilter _ [] = return [] -- optimisation
		runfilter a l = bloomFilter show l <$> genBloomFilter show a
		firstlevel = withKeysReferencedM
		secondlevel = withKeysReferencedInGit

{- Finds items in the first, smaller list, that are not
 - present in the second, larger list.
 - 
 - Constructing a single set, of the list that tends to be
 - smaller, appears more efficient in both memory and CPU
 - than constructing and taking the S.difference of two sets. -}
exclude :: Ord a => [a] -> [a] -> [a]
exclude [] _ = [] -- optimisation
exclude smaller larger = S.toList $ remove larger $ S.fromList smaller
	where
		remove a b = foldl (flip S.delete) b a

{- A bloom filter capable of holding half a million keys with a
 - false positive rate of 1 in 1000 uses around 8 mb of memory,
 - so will easily fit on even my lowest memory systems.
 -}
bloomCapacity :: Annex Int
bloomCapacity = fromMaybe 500000 . readish
	<$> getConfig "annex.bloomcapacity" ""
bloomAccuracy :: Annex Int
bloomAccuracy = fromMaybe 1000 . readish
	<$> getConfig "annex.bloomaccuracy" ""
bloomBitsHashes :: Annex (Int, Int)
bloomBitsHashes = do
	capacity <- bloomCapacity
	accuracy <- bloomAccuracy
	return $ suggestSizing capacity (1/ fromIntegral accuracy)

{- Creates a bloom filter, and runs an action, such as withKeysReferenced,
 - to populate it.
 -
 - The action is passed a callback that it can use to feed values into the
 - bloom filter. 
 -
 - Once the action completes, the mutable filter is frozen
 - for later use.
 -}
genBloomFilter :: Hashable t => (v -> t) -> ((v -> Annex ()) -> Annex b) -> Annex (Bloom t)
genBloomFilter convert populate = do
	(numbits, numhashes) <- bloomBitsHashes
	bloom <- lift $ newMB (cheapHashes numhashes) numbits
	_ <- populate $ \v -> lift $ insertMB bloom (convert v)
	lift $ unsafeFreezeMB bloom
	where
		lift = liftIO . stToIO

bloomFilter :: Hashable t => (v -> t) -> [v] -> Bloom t -> [v]
bloomFilter convert l bloom = filter (\k -> convert k `notElemB` bloom) l

{- Given an initial value, folds it with each key referenced by
 - symlinks in the git repo. -}
withKeysReferenced :: v -> (Key -> v -> v) -> Annex v
withKeysReferenced initial a = withKeysReferenced' initial folda
	where
		folda k v = return $ a k v

{- Runs an action on each referenced key in the git repo. -}
withKeysReferencedM :: (Key -> Annex ()) -> Annex ()
withKeysReferencedM a = withKeysReferenced' () calla
	where
		calla k _ = a k

withKeysReferenced' :: v -> (Key -> v -> Annex v) -> Annex v
withKeysReferenced' initial a = go initial =<< files
	where
		files = do
			top <- fromRepo Git.workTree
			inRepo $ LsFiles.inRepo [top]
		go v [] = return v
		go v (f:fs) = do
			x <- Backend.lookupFile f
			case x of
				Nothing -> go v fs
				Just (k, _) -> do
					!v' <- a k v
					go v' fs


withKeysReferencedInGit :: (Key -> Annex ()) -> Annex ()
withKeysReferencedInGit a = do
	rs <- relevantrefs <$> showref
	forM_ rs (withKeysReferencedInGitRef a)
	where
		showref = inRepo $ Git.Command.pipeRead [Param "show-ref"]
		relevantrefs = map (Git.Ref .  snd) .
			nubBy uniqref .
			filter ourbranches .
			map (separate (== ' ')) . lines
		uniqref (x, _) (y, _) = x == y
		ourbranchend = '/' : show Annex.Branch.name
		ourbranches (_, b) = not $ ourbranchend `isSuffixOf` b

withKeysReferencedInGitRef :: (Key -> Annex ()) -> Git.Ref -> Annex ()
withKeysReferencedInGitRef a ref = do
	showAction $ "checking " ++ Git.Ref.describe ref
	go =<< inRepo (LsTree.lsTree ref)
	where
		go [] = noop
		go (l:ls)
			| isSymLink (LsTree.mode l) = do
				content <- L.decodeUtf8 <$> catFile ref (LsTree.file l)
				case fileKey (takeFileName $ L.unpack content) of
					Nothing -> go ls
					Just k -> do
						a k
						go ls
			| otherwise = go ls

{- Looks in the specified directory for bad/tmp keys, and returns a list
 - of those that might still have value, or might be stale and removable.
 - 
 - Also, stale keys that can be proven to have no value are deleted.
 -}
staleKeysPrune :: (Git.Repo -> FilePath) -> Annex [Key]
staleKeysPrune dirspec = do
	contents <- staleKeys dirspec
	
	dups <- filterM inAnnex contents
	let stale = contents `exclude` dups

	dir <- fromRepo dirspec
	liftIO $ forM_ dups $ \t -> removeFile $ dir </> keyFile t

	return stale

staleKeys :: (Git.Repo -> FilePath) -> Annex [Key]
staleKeys dirspec = do
	dir <- fromRepo dirspec
	ifM (liftIO $ doesDirectoryExist dir)
		( do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM doesFileExist $
				map (dir </>) contents
			return $ mapMaybe (fileKey . takeFileName) files
		, return []
		)
