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
import Logs.PreferredContent
import Config.NumCopies
import Annex.Content
import Annex.Wanted
import Annex.Notification

import qualified Data.Set as S

def :: [Command]
def = [withOptions [dropFromOption] $ command "drop" paramPaths seek
	SectionCommon "indicate content of files not currently wanted"]

dropFromOption :: Option
dropFromOption = fieldOption ['f'] "from" paramRemote "drop content from a remote"

seek :: CommandSeek
seek ps = do
	from <- getOptionField dropFromOption Remote.byNameWithUUID
	withFilesInGit (whenAnnexed $ start from) ps

start :: Maybe Remote -> FilePath -> Key -> CommandStart
start from file key = checkDropAuto from file key $ \numcopies ->
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
	next $ performLocal key afile numcopies knownpresentremote

startRemote :: AssociatedFile -> NumCopies -> Key -> Remote -> CommandStart
startRemote afile numcopies key remote = do
	showStart' ("drop " ++ Remote.name remote) key afile
	next $ performRemote key afile numcopies remote

performLocal :: Key -> AssociatedFile -> NumCopies -> Maybe Remote -> CommandPerform
performLocal key afile numcopies knownpresentremote = lockContent key $ do
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	let trusteduuids' = case knownpresentremote of
		Nothing -> trusteduuids
		Just r -> nub (Remote.uuid r:trusteduuids)
	untrusteduuids <- trustGet UnTrusted
	let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids'++untrusteduuids)
	u <- getUUID
	ifM (canDrop u key afile numcopies trusteduuids' tocheck [])
		( do
			removeAnnex key
			notifyDrop afile True
			next $ cleanupLocal key
		, do
			notifyDrop afile False
			stop
		)

performRemote :: Key -> AssociatedFile -> NumCopies -> Remote -> CommandPerform
performRemote key afile numcopies remote = lockContent key $ do
	-- Filter the remote it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	-- When the local repo has the key, that's one additional copy,
	-- as long asthe local repo is not untrusted.
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	present <- inAnnex key
	u <- getUUID
	trusteduuids' <- if present
		then ifM ((<= SemiTrusted) <$> lookupTrust u)
			( pure (u:trusteduuids)
			, pure trusteduuids
			)
		else pure trusteduuids
	let have = filter (/= uuid) trusteduuids'
	untrusteduuids <- trustGet UnTrusted
	let tocheck = filter (/= remote) $
		Remote.remotesWithoutUUID remotes (have++untrusteduuids)
	stopUnless (canDrop uuid key afile numcopies have tocheck [uuid]) $ do
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
 - some locations where the key is known/assumed to be present.
 -
 - Also checks if it's required content, and refuses to drop if so.
 -
 - --force overrides and always allows dropping.
 -}
canDrop :: UUID -> Key -> AssociatedFile -> NumCopies -> [UUID] -> [Remote] -> [UUID] -> Annex Bool
canDrop dropfrom key afile numcopies have check skip = ifM (Annex.getState Annex.force)
	( return True
	, checkRequiredContent dropfrom key afile
		<&&>
	  findCopies key numcopies skip have check
	)

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

checkRequiredContent :: UUID -> Key -> AssociatedFile -> Annex Bool
checkRequiredContent u k afile =
	ifM (isRequiredContent (Just u) S.empty (Just k) afile False)
		( requiredContent
		, return True
		)

requiredContent :: Annex Bool
requiredContent = do
	showLongNote "That file is required content, it cannot be dropped!"
	showLongNote "(Use --force to override this check, or adjust required content configuration.)"
	return False

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
