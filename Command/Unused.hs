{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Unused where

import Control.Monad (filterM, unless, forM_)
import Control.Monad.State (liftIO)
import qualified Data.Set as S
import Data.Maybe
import System.FilePath
import System.Directory

import Command
import Types
import Content
import Messages
import Locations
import Utility
import LocationLog
import qualified Annex
import qualified Git
import qualified Git.LsFiles as LsFiles
import qualified Backend
import qualified Remote

command :: [Command]
command = [repoCommand "unused" paramNothing seek
	"look for unused file content"]

seek :: [CommandSeek]
seek = [withNothing start]

{- Finds unused content in the annex. -} 
start :: CommandStartNothing
start = notBareRepo $ do
	from <- Annex.getState Annex.fromremote
	let (name, action) = case from of
		Nothing -> (".", checkUnused)
		Just "." -> (".", checkUnused)
		Just n -> (n, checkRemoteUnused n)
	showStart "unused" name
	next action

checkUnused :: CommandPerform
checkUnused = do
	(unused, stalebad, staletmp) <- unusedKeys
	_ <- list "" unusedMsg unused 0 >>=
		list "bad" staleBadMsg stalebad >>=
			list "tmp" staleTmpMsg staletmp
	next $ return True
	where
		list file msg l c = do
			let unusedlist = number c l
			unless (null l) $ do
				showLongNote $ msg unusedlist
				showLongNote "\n"
			writeUnusedFile file unusedlist
			return $ c + length l

checkRemoteUnused :: String -> CommandPerform
checkRemoteUnused name = do
	checkRemoteUnused' =<< Remote.byName name
	next $ return True

checkRemoteUnused' :: Remote.Remote Annex -> Annex ()
checkRemoteUnused' r = do
	showAction "checking for unused data"
	referenced <- getKeysReferenced
	remotehas <- filterM isthere =<< loggedKeys
	let remoteunused = remotehas `exclude` referenced
	let list = number 0 remoteunused
	writeUnusedFile "" list
	unless (null remoteunused) $ do
		showLongNote $ remoteUnusedMsg r list
		showLongNote "\n"
	where
		isthere k = do
			us <- keyLocations k
			return $ uuid `elem` us
		uuid = Remote.uuid r

writeUnusedFile :: FilePath -> [(Int, Key)] -> Annex ()
writeUnusedFile prefix l = do
	g <- Annex.gitRepo
	liftIO $ viaTmp writeFile (gitAnnexUnusedLog prefix g) $
		unlines $ map (\(n, k) -> show n ++ " " ++ show k) l

table :: [(Int, Key)] -> [String]
table l = "  NUMBER  KEY" : map cols l
	where
		cols (n,k) = "  " ++ pad 6 (show n) ++ "  " ++ show k
		pad n s = s ++ replicate (n - length s) ' '

number :: Int -> [a] -> [(Int, a)]
number _ [] = []
number n (x:xs) = (n+1, x) : number (n+1) xs

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
	["Some annexed data is no longer used by any files in the current branch:"]
	[dropMsg Nothing,
	"Please be cautious -- are you sure that another branch, or another",
	"repository does not still use this data?"]

remoteUnusedMsg :: Remote.Remote Annex -> [(Int, Key)] -> String
remoteUnusedMsg r u = unusedMsg' u
	["Some annexed data on " ++ name ++ 
	 " is not used by any files in the current branch:"]
	[dropMsg $ Just r,
	 "Please be cautious -- Are you sure that the remote repository",
	 "does not use this data? Or that it's not used by another branch?"]
	where
		name = Remote.name r 

unusedMsg' :: [(Int, Key)] -> [String] -> [String] -> String
unusedMsg' u header trailer = unlines $
	header ++
	table u ++
	["(To see where data was previously used, try: git log --stat -S'KEY')"] ++
	trailer

dropMsg :: Maybe (Remote.Remote Annex) -> String
dropMsg Nothing = dropMsg' ""
dropMsg (Just r) = dropMsg' $ " --from " ++ Remote.name r
dropMsg' :: String -> String
dropMsg' s = "\nTo remove unwanted data: git-annex dropunused" ++ s ++ " NUMBER\n"

{- Finds keys whose content is present, but that do not seem to be used
 - by any files in the git repo, or that are only present as bad or tmp
 - files. -}
unusedKeys :: Annex ([Key], [Key], [Key])
unusedKeys = do
	fast <- Annex.getState Annex.fast
	if fast
		then do
			showNote "fast mode enabled; only finding stale files"
			tmp <- staleKeys gitAnnexTmpDir
			bad <- staleKeys gitAnnexBadDir
			return ([], bad, tmp)
		else do
			showAction "checking for unused data"
			present <- getKeysPresent
			referenced <- getKeysReferenced
			let unused = present `exclude` referenced
			staletmp <- staleKeysPrune gitAnnexTmpDir present
			stalebad <- staleKeysPrune gitAnnexBadDir present
			return (unused, stalebad, staletmp)

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

{- List of keys referenced by symlinks in the git repo. -}
getKeysReferenced :: Annex [Key]
getKeysReferenced = do
	g <- Annex.gitRepo
	files <- liftIO $ LsFiles.inRepo g [Git.workTree g]
	keypairs <- mapM Backend.lookupFile files
	return $ map fst $ catMaybes keypairs

{- Looks in the specified directory for bad/tmp keys, and returns a list
 - of those that might still have value, or might be stale and removable. 
 - 
 - When a list of presently available keys is provided, stale keys
 - that no longer have value are deleted.
 -}
staleKeysPrune :: (Git.Repo -> FilePath) -> [Key] -> Annex [Key]
staleKeysPrune dirspec present = do
	contents <- staleKeys dirspec
		
	let stale = contents `exclude` present
	let dup = contents `exclude` stale

	g <- Annex.gitRepo
	let dir = dirspec g
	liftIO $ forM_ dup $ \t -> removeFile $ dir </> keyFile t

	return stale

staleKeys :: (Git.Repo -> FilePath) -> Annex [Key]
staleKeys dirspec = do
	g <- Annex.gitRepo
	let dir = dirspec g
	exists <- liftIO $ doesDirectoryExist dir
	if not exists
		then return []
		else do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM doesFileExist $
				map (dir </>) contents
			return $ mapMaybe (fileKey . takeFileName) files
