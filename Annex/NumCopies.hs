{- git-annex numcopies configuration and checking
 -
 - Copyright 2014-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

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
	knownCopies,
) where

import Common.Annex
import qualified Annex
import Types.NumCopies
import Logs.NumCopies
import Logs.Trust
import Annex.CheckAttr
import qualified Remote
import qualified Types.Remote as Remote
import Annex.UUID
import Annex.Content

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
	-> [Remote] -- remotes to check to see if they have copies
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
	helper bad missing have (r:rs)
		| isSafeDrop need have = do
			p <- liftIO $ mkSafeDropProof need have
			case p of
				Right proof -> dropaction proof
				Left stillhave -> helper bad missing stillhave (r:rs)
		| otherwise = case Remote.lockContent r of
			Nothing -> fallback
			Just lockcontent -> lockcontent key $ \v -> case v of
				Nothing -> fallback
				Just vc -> helper bad missing (vc : have) rs
		  where
			fallback = do
				haskey <- Remote.hasKey r key
				case haskey of
					Right True  -> helper bad missing (mkVerifiedCopy RecentlyVerifiedCopy r : have) rs
					Left _      -> helper (r:bad) missing have rs
					Right False -> helper bad (Remote.uuid r:missing) have rs

notEnoughCopies :: Key -> NumCopies -> [VerifiedCopy] -> [UUID] -> [Remote] -> String -> Annex ()
notEnoughCopies key need have skip bad nolocmsg = do
	showNote "unsafe"
	showLongNote $
		"Could only verify the existence of " ++
		show (length have) ++ " out of " ++ show (fromNumCopies need) ++ 
		" necessary copies"
	Remote.showTriedRemotes bad
	Remote.showLocations True key (map toUUID have++skip) nolocmsg

{- Cost ordered lists of remotes that the location log indicates
 - may have a key.
 -
 - Also returns a list of UUIDs that are trusted to have the key
 - (some may not have configured remotes). If the current repository
 - currently has the key, and is not untrusted, it is included in this list.
 -}
knownCopies :: Key -> Annex ([Remote], [UUID])
knownCopies key = do
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	u <- getUUID
	trusteduuids' <- ifM (inAnnex key <&&> (<= SemiTrusted) <$> lookupTrust u)
		( pure (u:trusteduuids)
		, pure trusteduuids
		)
	return (remotes, trusteduuids')
