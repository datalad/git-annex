{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Drop where

import Common.Annex
import Command
import qualified Remote
import qualified Annex
import Annex.UUID
import Logs.Location
import Logs.Trust
import Config.NumCopies
import Annex.Content
import qualified Option
import Annex.Wanted

def :: [Command]
def = [withOptions [fromOption] $ command "drop" paramPaths seek
	SectionCommon "indicate content of files not currently wanted"]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "drop content from a remote"

seek :: CommandSeek
seek ps = do
	from <- getOptionField fromOption Remote.byNameWithUUID
	withFilesInGit (whenAnnexed $ start from) ps

start :: Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start from file (key, _) = checkDropAuto from file key $ \numcopies ->
	stopUnless (checkAuto $ wantDrop False (Remote.uuid <$> from) (Just key) (Just file)) $
		case from of
			Nothing -> startLocal (Just file) numcopies key Nothing
			Just remote -> do
				u <- getUUID
				if Remote.uuid remote == u
					then startLocal (Just file) numcopies key Nothing
					else startRemote (Just file) numcopies key remote

startLocal :: AssociatedFile -> NumCopies -> Key -> Maybe Remote -> CommandStart
startLocal afile numcopies key knownpresentremote = stopUnless (inAnnex key) $ do
	showStart' "drop" key afile
	next $ performLocal key numcopies knownpresentremote

startRemote :: AssociatedFile -> NumCopies -> Key -> Remote -> CommandStart
startRemote afile numcopies key remote = do
	showStart' ("drop " ++ Remote.name remote) key afile
	next $ performRemote key numcopies remote

performLocal :: Key -> NumCopies -> Maybe Remote -> CommandPerform
performLocal key numcopies knownpresentremote = lockContent key $ do
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	let trusteduuids' = case knownpresentremote of
		Nothing -> trusteduuids
		Just r -> nub (Remote.uuid r:trusteduuids)
	untrusteduuids <- trustGet UnTrusted
	let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids'++untrusteduuids)
	stopUnless (canDropKey key numcopies trusteduuids' tocheck []) $ do
		removeAnnex key
		next $ cleanupLocal key

performRemote :: Key -> NumCopies -> Remote -> CommandPerform
performRemote key numcopies remote = lockContent key $ do
	-- Filter the remote it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	-- When the local repo has the key, that's one additional copy.
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	present <- inAnnex key
	u <- getUUID
	let have = filter (/= uuid) $
		if present then u:trusteduuids else trusteduuids
	untrusteduuids <- trustGet UnTrusted
	let tocheck = filter (/= remote) $
		Remote.remotesWithoutUUID remotes (have++untrusteduuids)
	stopUnless (canDropKey key numcopies have tocheck [uuid]) $ do
		ok <- Remote.removeKey remote key
		next $ cleanupRemote key remote ok
  where
	uuid = Remote.uuid remote

cleanupLocal :: Key -> CommandCleanup
cleanupLocal key = do
	logStatus key InfoMissing
	return True

cleanupRemote :: Key -> Remote -> Bool -> CommandCleanup
cleanupRemote key remote ok = do
	when ok $
		Remote.logStatus remote key InfoMissing
	return ok

{- Checks specified remotes to verify that enough copies of a key exist to
 - allow it to be safely removed (with no data loss). Can be provided with
 - some locations where the key is known/assumed to be present. -}
canDropKey :: Key -> NumCopies -> [UUID] -> [Remote] -> [UUID] -> Annex Bool
canDropKey key numcopies have check skip = do
	force <- Annex.getState Annex.force
	if force || numcopies == NumCopies 0
		then return True
		else findCopies key numcopies skip have check

findCopies :: Key -> NumCopies -> [UUID] -> [UUID] -> [Remote] -> Annex Bool
findCopies key need skip = helper [] []
  where
	helper bad missing have []
		| NumCopies (length have) >= need = return True
		| otherwise = notEnoughCopies key need have (skip++missing) bad
	helper bad missing have (r:rs)
		| NumCopies (length have) >= need = return True
		| otherwise = do
			let u = Remote.uuid r
			let duplicate = u `elem` have
			haskey <- Remote.hasKey r key
			case (duplicate, haskey) of
				(False, Right True)  -> helper bad missing (u:have) rs
				(False, Left _)      -> helper (r:bad) missing have rs
				(False, Right False) -> helper bad (u:missing) have rs
				_                    -> helper bad missing have rs

notEnoughCopies :: Key -> NumCopies -> [UUID] -> [UUID] -> [Remote] -> Annex Bool
notEnoughCopies key need have skip bad = do
	unsafe
	showLongNote $
		"Could only verify the existence of " ++
		show (length have) ++ " out of " ++ show (fromNumCopies need) ++ 
		" necessary copies"
	Remote.showTriedRemotes bad
	Remote.showLocations key (have++skip)
		"Rather than dropping this file, try using: git annex move"
	hint
	return False
  where
	unsafe = showNote "unsafe"
	hint = showLongNote "(Use --force to override this check, or adjust numcopies.)"

{- In auto mode, only runs the action if there are enough
 - copies on other semitrusted repositories. -}
checkDropAuto :: Maybe Remote -> FilePath -> Key -> (NumCopies -> CommandStart) -> CommandStart
checkDropAuto mremote file key a = do
	numcopies <- getFileNumCopies file
	Annex.getState Annex.auto >>= auto numcopies
  where
	auto numcopies False = a numcopies
	auto numcopies True = do
		locs <- Remote.keyLocations key
		uuid <- getUUID
		let remoteuuid = fromMaybe uuid $ Remote.uuid <$> mremote
		locs' <- trustExclude UnTrusted $ filter (/= remoteuuid) locs
		if NumCopies (length locs') >= numcopies
			then a numcopies
			else stop
