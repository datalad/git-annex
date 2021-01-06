{- git-annex numcopies configuration and checking
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Annex.NumCopies (
	module Types.NumCopies,
	module Logs.NumCopies,
	getFileNumMinCopies,
	getAssociatedFileNumMinCopies,
	getGlobalFileNumCopies,
	getNumCopies,
	getMinCopies,
	deprecatedNumCopies,
	defaultNumCopies,
	numCopiesCheck,
	numCopiesCheck',
	verifyEnoughCopiesToDrop,
	verifiableCopies,
	UnVerifiedCopy(..),
) where

import Annex.Common
import qualified Annex
import Types.NumCopies
import Logs.NumCopies
import Logs.Trust
import Annex.CheckAttr
import qualified Remote
import qualified Types.Remote as Remote
import Annex.Content
import Annex.UUID

import Control.Exception
import qualified Control.Monad.Catch as M
import Data.Typeable

defaultNumCopies :: NumCopies
defaultNumCopies = NumCopies 1

defaultMinCopies :: MinCopies
defaultMinCopies = MinCopies 1

fromSourcesOr :: v -> [Annex (Maybe v)] -> Annex v
fromSourcesOr v = fromMaybe v <$$> getM id

{- The git config annex.numcopies is deprecated. -}
deprecatedNumCopies :: Annex (Maybe NumCopies)
deprecatedNumCopies = annexNumCopies <$> Annex.getGitConfig

{- Value forced on the command line by --numcopies. -}
getForcedNumCopies :: Annex (Maybe NumCopies)
getForcedNumCopies = Annex.getState Annex.forcenumcopies

{- Value forced on the command line by --mincopies. -}
getForcedMinCopies :: Annex (Maybe MinCopies)
getForcedMinCopies = Annex.getState Annex.forcemincopies

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

getAssociatedFileNumMinCopies :: AssociatedFile -> Annex (NumCopies, MinCopies)
getAssociatedFileNumMinCopies (AssociatedFile (Just file)) =
	getFileNumMinCopies file
getAssociatedFileNumMinCopies (AssociatedFile Nothing) = (,)
	<$> getNumCopies
	<*> getMinCopies

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
			( NumCopies <$> readish n
			, MinCopies <$> readish m
			)
		_ -> error "internal"

{- Checks if numcopies are satisfied for a file by running a comparison
 - between the number of (not untrusted) copies that are
 - belived to exist, and the configured value.
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
	NumCopies needed <- fst <$> getFileNumMinCopies file
	return $ length have `vs` needed

data UnVerifiedCopy = UnVerifiedRemote Remote | UnVerifiedHere
	deriving (Ord, Eq)

{- Verifies that enough copies of a key exist amoung the listed remotes,
 - to safely drop it, running an action with a proof if so, and
 - printing an informative message if not.
 -}
verifyEnoughCopiesToDrop
	:: String -- message to print when there are no known locations
	-> Key
	-> Maybe ContentRemovalLock
	-> NumCopies
	-> MinCopies
	-> [UUID] -- repos to skip considering (generally untrusted remotes)
	-> [VerifiedCopy] -- copies already verified to exist
	-> [UnVerifiedCopy] -- places to check to see if they have copies
	-> (SafeDropProof -> Annex a) -- action to perform the drop
	-> Annex a -- action to perform when unable to drop
	-> Annex a
verifyEnoughCopiesToDrop nolocmsg key removallock neednum needmin skip preverified tocheck dropaction nodropaction = 
	helper [] [] preverified (nub tocheck) []
  where
	helper bad missing have [] lockunsupported =
		liftIO (mkSafeDropProof neednum needmin have removallock) >>= \case
			Right proof -> dropaction proof
			Left stillhave -> do
				notEnoughCopies key neednum needmin stillhave (skip++missing) bad nolocmsg lockunsupported
				nodropaction
	helper bad missing have (c:cs) lockunsupported
		| isSafeDrop neednum needmin have removallock =
			liftIO (mkSafeDropProof neednum needmin have removallock) >>= \case
				Right proof -> dropaction proof
				Left stillhave -> helper bad missing stillhave (c:cs) lockunsupported
		| otherwise = case c of
			UnVerifiedHere -> lockContentShared key contverified
			UnVerifiedRemote r -> checkremote r contverified $
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
			a `M.catches`
				[ M.Handler (\ (e :: AsyncException) -> throwM e)
				, M.Handler (\ (e :: SomeAsyncException) -> throwM e)
				, M.Handler (\ (DropException e') -> throwM e')
				, M.Handler (\ (_e :: SomeException) -> fallback)
				]
		Nothing -> fallback

data DropException = DropException SomeException
	deriving (Typeable, Show)

instance Exception DropException

notEnoughCopies :: Key -> NumCopies -> MinCopies -> [VerifiedCopy] -> [UUID] -> [Remote] -> String -> [Remote] -> Annex ()
notEnoughCopies key neednum needmin have skip bad nolocmsg lockunsupported = do
	showNote "unsafe"
	if length have < fromNumCopies neednum
		then showLongNote $
			"Could only verify the existence of " ++
			show (length have) ++ " out of " ++ show (fromNumCopies neednum) ++ 
			" necessary copies"
		else do
			showLongNote $ "Unable to lock down " ++ show (fromMinCopies needmin) ++ " copy of file that is required to safely drop it."
			if null lockunsupported
				then showLongNote "(This could have happened because of a concurrent drop, or because a remote has too old a version of git-annex-shell installed.)"
				else showLongNote $ "These remotes do not support locking: "
					++ Remote.listRemoteNames lockunsupported

	Remote.showTriedRemotes bad
	Remote.showLocations True key (map toUUID have++skip) nolocmsg

{- Finds locations of a key that can be used to get VerifiedCopies,
 - in order to allow dropping the key.
 -
 - Provide a list of UUIDs that the key is being dropped from.
 - The returned lists will exclude any of those UUIDs.
 -
 - The return lists also exclude any repositories that are untrusted,
 - since those should not be used for verification.
 -
 - The UnVerifiedCopy list is cost ordered.
 - The VerifiedCopy list contains repositories that are trusted to
 - contain the key.
 -}
verifiableCopies :: Key -> [UUID] -> Annex ([UnVerifiedCopy], [VerifiedCopy])
verifiableCopies key exclude = do
	locs <- Remote.keyLocations key
	(remotes, trusteduuids) <- Remote.remoteLocations locs
		=<< trustGet Trusted
	untrusteduuids <- trustGet UnTrusted
	let exclude' = exclude ++ untrusteduuids
	let remotes' = Remote.remotesWithoutUUID remotes (exclude' ++ trusteduuids)
	let verified = map (mkVerifiedCopy TrustedCopy) $
		filter (`notElem` exclude') trusteduuids
	u <- getUUID
	let herec = if u `elem` locs && u `notElem` exclude'
		then [UnVerifiedHere]
		else []
	return (herec ++ map UnVerifiedRemote remotes', verified)
