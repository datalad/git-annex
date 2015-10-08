{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
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
import Annex.NumCopies
import Annex.Content
import Annex.Wanted
import Annex.Notification

import qualified Data.Set as S

cmd :: Command
cmd = withGlobalOptions annexedMatchingOptions $
	command "drop" SectionCommon
		"remove content of files from repository"
		paramPaths (seek <$$> optParser)

data DropOptions = DropOptions
	{ dropFiles :: CmdParams
	, dropFrom :: Maybe (DeferredParse Remote)
	, autoMode :: Bool
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser DropOptions
optParser desc = DropOptions
	<$> cmdParams desc
	<*> optional parseDropFromOption
	<*> parseAutoOption
	<*> optional (parseKeyOptions False)

parseDropFromOption :: Parser (DeferredParse Remote)
parseDropFromOption = parseRemoteOption $ strOption
	( long "from" <> short 'f' <> metavar paramRemote
	<> help "drop content from a remote"
	<> completeRemotes
	)

seek :: DropOptions -> CommandSeek
seek o = withKeyOptions (keyOptions o) (autoMode o)
	(startKeys o)
	(withFilesInGit $ whenAnnexed $ start o)
	(dropFiles o)

start :: DropOptions -> FilePath -> Key -> CommandStart
start o file key = start' o key (Just file)

start' :: DropOptions -> Key -> AssociatedFile -> CommandStart
start' o key afile = do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (dropFrom o)
	checkDropAuto (autoMode o) from afile key $ \numcopies ->
		stopUnless (want from) $
			case from of
				Nothing -> startLocal afile numcopies key Nothing
				Just remote -> do
					u <- getUUID
					if Remote.uuid remote == u
						then startLocal afile numcopies key Nothing
						else startRemote afile numcopies key remote
	  where
		want from
			| autoMode o = wantDrop False (Remote.uuid <$> from) (Just key) afile
			| otherwise = return True

startKeys :: DropOptions -> Key -> CommandStart
startKeys o key = start' o key Nothing

startLocal :: AssociatedFile -> NumCopies -> Key -> Maybe Remote -> CommandStart
startLocal afile numcopies key knownpresentremote = stopUnless (inAnnex key) $ do
	showStart' "drop" key afile
	next $ performLocal key afile numcopies knownpresentremote

startRemote :: AssociatedFile -> NumCopies -> Key -> Remote -> CommandStart
startRemote afile numcopies key remote = do
	showStart' ("drop " ++ Remote.name remote) key afile
	next $ performRemote key afile numcopies remote

-- Note that lockContentExclusive is called before checking if the key is
-- present on enough remotes to allow removal. This avoids a scenario where two
-- or more remotes are trying to remove a key at the same time, and each
-- sees the key is present on the other.
performLocal :: Key -> AssociatedFile -> NumCopies -> Maybe Remote -> CommandPerform
performLocal key afile numcopies knownpresentremote = lockContentExclusive key $ \contentlock -> do
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	let trusteduuids' = case knownpresentremote of
		Nothing -> trusteduuids
		Just r -> Remote.uuid r:trusteduuids
	untrusteduuids <- trustGet UnTrusted
	let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids'++untrusteduuids)
	u <- getUUID
	ifM (canDrop u key afile numcopies trusteduuids' tocheck [])
		( do
			removeAnnex contentlock
			notifyDrop afile True
			next $ cleanupLocal key
		, do
			notifyDrop afile False
			stop
		)

performRemote :: Key -> AssociatedFile -> NumCopies -> Remote -> CommandPerform
performRemote key afile numcopies remote = do
	-- Filter the remote it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	-- When the local repo has the key, that's one additional copy,
	-- as long as the local repo is not untrusted.
	(remotes, trusteduuids) <- knownCopies key
	let have = filter (/= uuid) trusteduuids
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
canDrop dropfrom key afile numcopies have check skip = 
	ifM (Annex.getState Annex.force)
		( return True
			, ifM (checkRequiredContent dropfrom key afile
				<&&> verifyEnoughCopies nolocmsg key numcopies skip have check
				)
				( return True
				, do
					hint
					return False
				)
		)
  where
	nolocmsg = "Rather than dropping this file, try using: git annex move"
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
checkDropAuto :: Bool -> Maybe Remote -> AssociatedFile -> Key -> (NumCopies -> CommandStart) -> CommandStart
checkDropAuto automode mremote afile key a = go =<< maybe getNumCopies getFileNumCopies afile
  where
	go numcopies
		| automode = do
			locs <- Remote.keyLocations key
			uuid <- getUUID
			let remoteuuid = fromMaybe uuid $ Remote.uuid <$> mremote
			locs' <- trustExclude UnTrusted $ filter (/= remoteuuid) locs
			if NumCopies (length locs') >= numcopies
				then a numcopies
				else stop
		| otherwise = a numcopies
