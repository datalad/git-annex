{- git-annex numcopies configuration and checking
 -
 - Copyright 2014-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, OverloadedStrings #-}

module Annex.NumCopies (
	module Types.NumCopies,
	module Logs.NumCopies,
	getFileNumMinCopies,
	getSafestNumMinCopies,
	getSafestNumMinCopies',
	getGlobalFileNumCopies,
	getNumCopies,
	getMinCopies,
	deprecatedNumCopies,
	defaultNumCopies,
	numCopiesCheck,
	numCopiesCheck',
	numCopiesCheck'',
	numCopiesCount,
	verifyEnoughCopiesToDrop,
	verifiableCopies,
	UnVerifiedCopy(..),
) where

import Annex.Common
import qualified Annex
import Types.NumCopies
import Logs.NumCopies
import Logs.Trust
import Logs.Cluster
import Annex.CheckAttr
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Content
import Annex.UUID
import Annex.CatFile
import qualified Database.Keys

import Control.Exception
import qualified Control.Monad.Catch as MC
import Data.Typeable
import qualified Data.Set as S
import qualified Data.Map as M

defaultNumCopies :: NumCopies
defaultNumCopies = configuredNumCopies 1

defaultMinCopies :: MinCopies
defaultMinCopies = configuredMinCopies 1

fromSourcesOr :: v -> [Annex (Maybe v)] -> Annex v
fromSourcesOr v = fromMaybe v <$$> getM id

{- The git config annex.numcopies is deprecated. -}
deprecatedNumCopies :: Annex (Maybe NumCopies)
deprecatedNumCopies = annexNumCopies <$> Annex.getGitConfig

{- Value forced on the command line by --numcopies. -}
getForcedNumCopies :: Annex (Maybe NumCopies)
getForcedNumCopies = Annex.getRead Annex.forcenumcopies

{- Value forced on the command line by --mincopies. -}
getForcedMinCopies :: Annex (Maybe MinCopies)
getForcedMinCopies = Annex.getRead Annex.forcemincopies

{- NumCopies value from any of the non-.gitattributes configuration
 - sources. -}
getNumCopies :: Annex NumCopies
getNumCopies = fromSourcesOr defaultNumCopies
	[ getForcedNumCopies
	, getGlobalNumCopies
	, deprecatedNumCopies
	]

{- MinCopies value from any of the non-.gitattributes configuration
 - sources. -}
getMinCopies :: Annex MinCopies
getMinCopies = fromSourcesOr defaultMinCopies
	[ getForcedMinCopies
	, getGlobalMinCopies
	]

{- NumCopies and MinCopies value for a file, from any configuration source,
 - including .gitattributes. -}
getFileNumMinCopies :: RawFilePath -> Annex (NumCopies, MinCopies)
getFileNumMinCopies f = do
	fnumc <- getForcedNumCopies
	fminc <- getForcedMinCopies
	case (fnumc, fminc) of
		(Just numc, Just minc) -> return (numc, minc)
		(Just numc, Nothing) -> do
			minc <- fromSourcesOr defaultMinCopies
				[ snd <$> getNumMinCopiesAttr f
				, getGlobalMinCopies
				]
			return (numc, minc)
		(Nothing, Just minc) -> do
			numc <- fromSourcesOr defaultNumCopies
				[ fst <$> getNumMinCopiesAttr f
				, getGlobalNumCopies
				, deprecatedNumCopies
				]
			return (numc, minc)
		(Nothing, Nothing) -> do
			let fallbacknum = fromSourcesOr defaultNumCopies
				[ getGlobalNumCopies
				, deprecatedNumCopies
				]
			let fallbackmin = fromSourcesOr defaultMinCopies
				[ getGlobalMinCopies
				]
			getNumMinCopiesAttr f >>= \case
				(Just numc, Just minc) -> 
					return (numc, minc)
				(Just numc, Nothing) -> (,)
					<$> pure numc
					<*> fallbackmin
				(Nothing, Just minc) -> (,)
					<$> fallbacknum
					<*> pure minc
				(Nothing, Nothing) -> (,)
					<$> fallbacknum
					<*> fallbackmin

{- Gets the highest NumCopies and MinCopies value for all files
 - associated with a key. Provide any known associated file;
 - the rest are looked up from the database.
 -
 - Using this when dropping, rather than getFileNumMinCopies
 - avoids dropping one file that has a smaller value violating
 - the value set for another file that uses the same content.
 -}
getSafestNumMinCopies :: AssociatedFile -> Key -> Annex (NumCopies, MinCopies)
getSafestNumMinCopies afile k =
	Database.Keys.getAssociatedFilesIncluding afile k
		>>= getSafestNumMinCopies' afile k

getSafestNumMinCopies' :: AssociatedFile -> Key -> [RawFilePath] -> Annex (NumCopies, MinCopies)
getSafestNumMinCopies' afile k fs = do
	l <- mapM getFileNumMinCopies fs
	let l' = zip l fs
	(,)
		<$> findmax fst l' getNumCopies
		<*> findmax snd l' getMinCopies
  where
	-- Some associated files in the keys database may no longer
	-- correspond to files in the repository.
	-- (But the AssociatedFile passed to this is known to be
	-- an associated file, which may not be in the keys database
	-- yet, so checking it is skipped.)
	stillassociated f
		| AssociatedFile (Just f) == afile = return True
		| otherwise = catKeyFile f >>= \case
			Just k' | k' == k -> return True
			_ -> return False
	
	-- Avoid calling stillassociated on every file; just make sure
	-- that the one with the highest value is still associated.
	findmax _ [] fallback = fallback
	findmax getv l fallback = do
		let n = maximum (map (getv . fst) l)
		let (maxls, l') = partition (\(x, _) -> getv x == n) l
		ifM (anyM stillassociated (map snd maxls))
			( return n
			, findmax getv l' fallback
			)

{- This is the globally visible numcopies value for a file. So it does
 - not include local configuration in the git config or command line
 - options. -}
getGlobalFileNumCopies :: RawFilePath  -> Annex NumCopies
getGlobalFileNumCopies f = fromSourcesOr defaultNumCopies
	[ fst <$> getNumMinCopiesAttr f
	, getGlobalNumCopies
	]

getNumMinCopiesAttr :: RawFilePath  -> Annex (Maybe NumCopies, Maybe MinCopies)
getNumMinCopiesAttr file =
	checkAttrs ["annex.numcopies", "annex.mincopies"] file >>= \case
		(n:m:[]) -> return
			( configuredNumCopies <$> readish n
			, configuredMinCopies <$> readish m
			)
		_ -> error "internal"

{- Checks if numcopies are satisfied for a file by running a comparison
 - between the number of (not untrusted) copies that are
 - believed to exist, and the configured value.
 -
 - This is good enough for everything except dropping the file, which
 - requires active verification of the copies.
 -}
numCopiesCheck :: RawFilePath -> Key -> (Int -> Int -> v) -> Annex v
numCopiesCheck file key vs = do
	have <- trustExclude UnTrusted =<< Remote.keyLocations key
	numCopiesCheck' file vs have

numCopiesCheck' :: RawFilePath -> (Int -> Int -> v) -> [UUID] -> Annex v
numCopiesCheck' file vs have = do
	needed <- fst <$> getFileNumMinCopies file
	let nhave = numCopiesCount have
	explain (ActionItemTreeFile file) $ Just $ UnquotedString $
		"has " ++ show nhave ++ " " ++ pluralCopies nhave ++ 
		", and the configured annex.numcopies is " ++ show needed
	return $ numCopiesCheck'' have vs needed

numCopiesCheck'' :: [UUID] -> (Int -> Int -> v) -> NumCopies -> v
numCopiesCheck'' have vs needed =
	let nhave = numCopiesCount have
	in nhave `vs` fromNumCopies needed

{- When a key is logged as present in a node of the cluster,
 - the cluster's UUID will also be in the list, but is not a
 - distinct copy.
 -}
numCopiesCount :: [UUID] -> Int
numCopiesCount = length . filter (not . isClusterUUID)

data UnVerifiedCopy = UnVerifiedRemote Remote | UnVerifiedHere
	deriving (Ord, Eq)

{- Verifies that enough copies of a key exist among the listed remotes,
 - to safely drop it, running an action with a proof if so, and
 - printing an informative message if not.
 -}
verifyEnoughCopiesToDrop
	:: String -- message to print when there are no known locations
	-> Key
	-> Maybe UUID -- repo dropping from
	-> Maybe ContentRemovalLock
	-> NumCopies
	-> MinCopies
	-> [UUID] -- repos to skip considering (generally untrusted remotes)
	-> [VerifiedCopy] -- copies already verified to exist
	-> [UnVerifiedCopy] -- places to check to see if they have copies
	-> (SafeDropProof -> Annex a) -- action to perform the drop
	-> Annex a -- action to perform when unable to drop
	-> Annex a
verifyEnoughCopiesToDrop nolocmsg key dropfrom removallock neednum needmin skip preverified tocheck dropaction nodropaction = 
	helper [] [] preverified (nub tocheck) []
  where
	helper bad missing have [] lockunsupported =
		liftIO (mkSafeDropProof neednum needmin have removallock) >>= \case
			Right proof -> dropaction proof
			Left stillhave -> do
				notEnoughCopies key dropfrom neednum needmin stillhave (skip++missing) bad nolocmsg lockunsupported
				nodropaction
	helper bad missing have (c:cs) lockunsupported
		| isSafeDrop neednum needmin have removallock =
			liftIO (mkSafeDropProof neednum needmin have removallock) >>= \case
				Right proof -> dropaction proof
				Left stillhave -> helper bad missing stillhave (c:cs) lockunsupported
		| otherwise = case c of
			UnVerifiedHere -> lockContentShared key contverified
			UnVerifiedRemote r
				-- Skip cluster uuids because locking is
				-- not supported with them, instead will
				-- lock individual nodes.
				| isClusterUUID (Remote.uuid r) -> helper bad missing have cs lockunsupported
				| otherwise -> checkremote r contverified $
					let lockunsupported' = r : lockunsupported
					in Remote.hasKey r key >>= \case
						Right True  -> helper bad missing (mkVerifiedCopy RecentlyVerifiedCopy r : have) cs lockunsupported'
						Left _      -> helper (r:bad) missing have cs lockunsupported'
						Right False -> helper bad (Remote.uuid r:missing) have cs lockunsupported'
		  where
			contverified vc = helper bad missing (vc : have) cs lockunsupported

	checkremote r cont fallback = case Remote.lockContent r of
		Just lockcontent -> do
			-- The remote's lockContent will throw an exception
			-- when it is unable to lock, in which case the
			-- fallback should be run. 
			--
			-- On the other hand, the continuation could itself
			-- throw an exception (ie, the eventual drop action
			-- fails), and in this case we don't want to run the
			-- fallback since part of the drop action may have
			-- already been performed.
			--
			-- Differentiate between these two sorts
			-- of exceptions by using DropException.
			let a = lockcontent key $ \v -> 
				cont v `catchNonAsync` (throw . DropException)
			a `MC.catches`
				[ MC.Handler (\ (e :: AsyncException) -> throwM e)
				, MC.Handler (\ (e :: SomeAsyncException) -> throwM e)
				, MC.Handler (\ (DropException e') -> throwM e')
				, MC.Handler (\ (_e :: SomeException) -> fallback)
				]
		Nothing -> fallback

data DropException = DropException SomeException
	deriving (Typeable, Show)

instance Exception DropException

notEnoughCopies :: Key -> Maybe UUID -> NumCopies -> MinCopies -> [VerifiedCopy] -> [UUID] -> [Remote] -> String -> [Remote] -> Annex ()
notEnoughCopies key dropfrom neednum needmin have skip bad nolocmsg lockunsupported = do
	showNote "unsafe"
	if length have < fromNumCopies neednum
		then showLongNote $ UnquotedString $
			if fromNumCopies neednum == 1
				then "Could not verify the existence of the 1 necessary copy."
				else "Could only verify the existence of " ++
					show (length have) ++ " out of " ++ show (fromNumCopies neednum) ++ 
					" necessary " ++ pluralCopies (fromNumCopies neednum) ++ "."
		else do
			showLongNote $ UnquotedString $ "Unable to lock down " ++ show (fromMinCopies needmin) ++ 
				" " ++ pluralCopies (fromMinCopies needmin) ++ 
				" of file necessary to safely drop it."
			if null lockunsupported
				then showLongNote "(This could have happened because of a concurrent drop, or because a remote has too old a version of git-annex-shell installed.)"
				else showLongNote $ UnquotedString $ "These remotes do not support locking: "
					++ Remote.listRemoteNames lockunsupported

	Remote.showTriedRemotes bad
	-- When dropping from a cluster, don't suggest making the nodes of
	-- the cluster available
	clusternodes <- case mkClusterUUID =<< dropfrom of
		Nothing -> pure []
		Just cu -> do
			clusters <- getClusters
			pure $ maybe [] (map fromClusterNodeUUID . S.toList) $
				M.lookup cu (clusterUUIDs clusters)
	let excludeset = S.fromList $ map toUUID have++skip++clusternodes
	-- Don't suggest making a cluster available when dropping from its
	-- node.
	let exclude u
		| u `S.member` excludeset = pure True
		| otherwise = case (dropfrom, mkClusterUUID u) of
			(Just dropfrom', Just cu) -> do
				clusters <- getClusters
				pure $ case M.lookup cu (clusterUUIDs clusters) of
					Just nodes -> 
						ClusterNodeUUID dropfrom' 
							`S.member` nodes
					Nothing -> False
			_ -> pure False
	Remote.showLocations True key exclude nolocmsg

pluralCopies :: Int -> String
pluralCopies 1 = "copy"
pluralCopies _ = "copies"

{- Finds locations of a key that can be used to get VerifiedCopies,
 - in order to allow dropping the key.
 -
 - Provide a list of UUIDs that the key is being dropped from.
 - The returned lists will exclude any of those UUIDs.
 -
 - The return lists also exclude any repositories that are untrusted,
 - since those should not be used for verification.
 -
 - When dropping from a cluster UUID, its nodes are excluded.
 -
 - Cluster UUIDs are also excluded since locking a key on a cluster
 - is done by locking on individual nodes.
 -
 - The UnVerifiedCopy list is cost ordered.
 - The VerifiedCopy list contains repositories that are trusted to
 - contain the key.
 -}
verifiableCopies :: Key -> [UUID] -> Annex ([UnVerifiedCopy], [VerifiedCopy])
verifiableCopies key exclude = do
	locs <- filter (not . isClusterUUID) <$> Remote.keyLocations key
	(remotes, trusteduuids) <- Remote.remoteLocations (Remote.IncludeIgnored False) locs
		=<< trustGet Trusted
	clusternodes <- if any isClusterUUID exclude
		then do
			clusters <- getClusters
			pure $ concatMap (getclusternodes clusters) exclude
		else pure []
	untrusteduuids <- trustGet UnTrusted
	let exclude' = exclude ++ untrusteduuids ++ clusternodes
	let remotes' = Remote.remotesWithoutUUID remotes (exclude' ++ trusteduuids)
	let verified = map (mkVerifiedCopy TrustedCopy) $
		filter (`notElem` exclude') trusteduuids
	u <- getUUID
	let herec = if u `elem` locs && u `notElem` exclude'
		then [UnVerifiedHere]
		else []
	return (herec ++ map UnVerifiedRemote remotes', verified)
  where
	getclusternodes clusters u = case mkClusterUUID u of
		Just cu -> maybe [] (map fromClusterNodeUUID . S.toList) $
			M.lookup cu (clusterUUIDs clusters)
		Nothing -> []
