{- git-annex numcopies configuration and checking
 -
 - Copyright 2014-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

module Annex.NumCopies (
	module Types.NumCopies,
	module Logs.NumCopies,
	getFileNumCopies,
	getGlobalFileNumCopies,
	getNumCopies,
	deprecatedNumCopies,
	defaultNumCopies,
	numCopiesCheck,
	numCopiesCheck',
	verifyEnoughCopiesToDrop,
	verifiableCopies,
	UnVerifiedCopy,
) where

import Common.Annex
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

fromSources :: [Annex (Maybe NumCopies)] -> Annex NumCopies
fromSources = fromMaybe defaultNumCopies <$$> getM id

{- The git config annex.numcopies is deprecated. -}
deprecatedNumCopies :: Annex (Maybe NumCopies)
deprecatedNumCopies = annexNumCopies <$> Annex.getGitConfig

{- Value forced on the command line by --numcopies. -}
getForcedNumCopies :: Annex (Maybe NumCopies)
getForcedNumCopies = Annex.getState Annex.forcenumcopies

{- Numcopies value from any of the non-.gitattributes configuration
 - sources. -}
getNumCopies :: Annex NumCopies
getNumCopies = fromSources
	[ getForcedNumCopies
	, getGlobalNumCopies
	, deprecatedNumCopies
	]

{- Numcopies value for a file, from any configuration source, including the
 - deprecated git config. -}
getFileNumCopies :: FilePath -> Annex NumCopies
getFileNumCopies f = fromSources
	[ getForcedNumCopies
	, getFileNumCopies' f
	, deprecatedNumCopies
	]

{- This is the globally visible numcopies value for a file. So it does
 - not include local configuration in the git config or command line
 - options. -}
getGlobalFileNumCopies :: FilePath  -> Annex NumCopies
getGlobalFileNumCopies f = fromSources
	[ getFileNumCopies' f
	]

getFileNumCopies' :: FilePath  -> Annex (Maybe NumCopies)
getFileNumCopies' file = maybe getGlobalNumCopies (return . Just) =<< getattr
  where
	getattr = (NumCopies <$$> readish)
		<$> checkAttr "annex.numcopies" file

{- Checks if numcopies are satisfied for a file by running a comparison
 - between the number of (not untrusted) copies that are
 - belived to exist, and the configured value.
 -
 - This is good enough for everything except dropping the file, which
 - requires active verification of the copies.
 -}
numCopiesCheck :: FilePath -> Key -> (Int -> Int -> v) -> Annex v
numCopiesCheck file key vs = do
	have <- trustExclude UnTrusted =<< Remote.keyLocations key
	numCopiesCheck' file vs have

numCopiesCheck' :: FilePath -> (Int -> Int -> v) -> [UUID] -> Annex v
numCopiesCheck' file vs have = do
	NumCopies needed <- getFileNumCopies file
	return $ length have `vs` needed

data UnVerifiedCopy = UnVerifiedRemote Remote | UnVerifiedHere
	deriving (Ord, Eq)

{- Verifies that enough copies of a key exist amoung the listed remotes,
 - running an action with a proof if so, and printing an informative
 - message if not.
 -}
verifyEnoughCopiesToDrop
	:: String -- message to print when there are no known locations
	-> Key
	-> NumCopies
	-> [UUID] -- repos to skip considering (generally untrusted remotes)
	-> [VerifiedCopy] -- copies already verified to exist
	-> [UnVerifiedCopy] -- places to check to see if they have copies
	-> (SafeDropProof -> Annex a) -- action to perform to drop
	-> Annex a -- action to perform when unable to drop
	-> Annex a
verifyEnoughCopiesToDrop nolocmsg key need skip preverified tocheck dropaction nodropaction = 
	helper [] [] preverified (nub tocheck)
  where
	helper bad missing have [] = do
		p <- liftIO $ mkSafeDropProof need have
		case p of
			Right proof -> dropaction proof
			Left stillhave -> do
				notEnoughCopies key need stillhave (skip++missing) bad nolocmsg
				nodropaction
	helper bad missing have (c:cs)
		| isSafeDrop need have = do
			p <- liftIO $ mkSafeDropProof need have
			case p of
				Right proof -> dropaction proof
				Left stillhave -> helper bad missing stillhave (c:cs)
		| otherwise = case c of
			UnVerifiedHere -> lockContentShared key contverified
			UnVerifiedRemote r -> checkremote r contverified $ do
				haskey <- Remote.hasKey r key
				case haskey of
					Right True  -> helper bad missing (mkVerifiedCopy RecentlyVerifiedCopy r : have) cs
					Left _      -> helper (r:bad) missing have cs
					Right False -> helper bad (Remote.uuid r:missing) have cs
		  where
			contverified vc = helper bad missing (vc : have) cs

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
				, M.Handler (\ (DropException e') -> throwM e')
				, M.Handler (\ (_e :: SomeException) -> fallback)
				]
		Nothing -> fallback

data DropException = DropException SomeException
	deriving (Typeable, Show)

instance Exception DropException

notEnoughCopies :: Key -> NumCopies -> [VerifiedCopy] -> [UUID] -> [Remote] -> String -> Annex ()
notEnoughCopies key need have skip bad nolocmsg = do
	showNote "unsafe"
	showLongNote $
		"Could only verify the existence of " ++
		show (length have) ++ " out of " ++ show (fromNumCopies need) ++ 
		" necessary copies"
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
