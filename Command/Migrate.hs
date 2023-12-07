{- git-annex command
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Migrate where

import Command
import Backend
import Types.Backend (canUpgradeKey, fastMigrate)
import Types.KeySource
import Annex.Content
import qualified Command.ReKey
import qualified Command.Fsck
import qualified Annex
import Logs.Migrate
import Logs.MetaData
import Logs.Web
import Logs.Location
import Utility.Metered

cmd :: Command
cmd = withAnnexOptions [backendOption, annexedMatchingOptions, jsonOptions] $
	command "migrate" SectionUtility 
		"switch data to different backend"
		paramPaths (seek <$$> optParser)

data MigrateOptions = MigrateOptions
	{ migrateThese :: CmdParams
	, updateOption :: Bool
	, removeSize :: Bool
	}

optParser :: CmdParamsDesc -> Parser MigrateOptions
optParser desc = MigrateOptions
	<$> cmdParams desc
	<*> switch
		( long "update"
		<> help "update for migrations performed elsewhere"
		)
	<*> switch
		( long "remove-size"
		<> help "remove size field from keys"
		)

seek :: MigrateOptions -> CommandSeek
seek o
	| updateOption o = do
		unless (null (migrateThese o)) $
			error "Cannot combine --update with files to migrate."
		commandAction update
	| otherwise = do
		withFilesInGitAnnex ww seeker =<< workTreeItems ww (migrateThese o)
		commitMigration
  where
	ww = WarnUnmatchLsFiles "migrate"
	seeker = AnnexedFileSeeker
		{ startAction = start o
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: MigrateOptions -> Maybe KeySha -> SeekInput -> RawFilePath -> Key -> CommandStart
start o ksha si file key = do
	forced <- Annex.getRead Annex.force
	v <- Backend.getBackend (fromRawFilePath file) key
	case v of
		Nothing -> stop
		Just oldbackend -> do
			exists <- inAnnex key
			newbackend <- chooseBackend file
			if (newbackend /= oldbackend || upgradableKey oldbackend key || forced) && exists
				then go False oldbackend newbackend
				else if removeSize o && exists
					then go True oldbackend oldbackend
					else stop
  where
	go onlyremovesize oldbackend newbackend = do
		keyrec <- case ksha of
			Just (KeySha s) -> pure (MigrationRecord s)
			Nothing -> error "internal"
		starting "migrate" (mkActionItem (key, file)) si $
			perform onlyremovesize o file key keyrec oldbackend newbackend

{- Checks if a key is upgradable to a newer representation.
 - 
 - Reasons for migration:
 -  - Ideally, all keys have file size metadata. Old keys may not.
 -  - Something has changed in the backend, such as a bug fix.
 -}
upgradableKey :: Backend -> Key -> Bool
upgradableKey backend key = isNothing (fromKey keySize key) || backendupgradable
  where
	backendupgradable = maybe False (\a -> a key) (canUpgradeKey backend)

{- Store the old backend's key in the new backend
 - The old backend's key is not dropped from it, because there may
 - be other files still pointing at that key.
 -
 - To ensure that the data we have for the old key is valid, it's
 - fscked here. First we generate the new key. This ensures that the
 - data cannot get corrupted after the fsck but before the new key is
 - generated.
 -}
perform :: Bool -> MigrateOptions -> RawFilePath -> Key -> MigrationRecord -> Backend -> Backend -> CommandPerform
perform onlyremovesize o file oldkey oldkeyrec oldbackend newbackend = go =<< genkey (fastMigrate oldbackend)
  where
	go Nothing = stop
	go (Just (newkey, knowngoodcontent))
		| knowngoodcontent = finish (removesize newkey)
		| otherwise = stopUnless checkcontent $
			finish (removesize newkey)
	checkcontent = Command.Fsck.checkBackend oldbackend oldkey KeyPresent afile
	finish newkey = ifM (Command.ReKey.linkKey file oldkey newkey)
		( do
			_ <- copyMetaData oldkey newkey
			-- If the old key had some associated urls, record them for
			-- the new key as well.
			urls <- getUrls oldkey
			forM_ urls $ \url ->
				setUrlPresent newkey url
			next $ Command.ReKey.cleanup file newkey $
				logMigration oldkeyrec
		, giveup "failed creating link from old to new key"
		)
	genkey _ | onlyremovesize = return $ Just (oldkey, False)
	genkey Nothing = do
		content <- calcRepo $ gitAnnexLocation oldkey
		let source = KeySource
			{ keyFilename = file
			, contentLocation = content
			, inodeCache = Nothing
			}
		newkey <- fst <$> genKey source nullMeterUpdate newbackend
		return $ Just (newkey, False)
	genkey (Just fm) = fm oldkey newbackend afile >>= \case
		Just newkey -> return (Just (newkey, True))
		Nothing -> genkey Nothing
	removesize k
		| removeSize o = alterKey k $ \kd -> kd { keySize = Nothing } 
		| otherwise = k
	afile = AssociatedFile (Just file)

update :: CommandStart
update = starting "migrate" (ActionItemOther Nothing) (SeekInput []) $ do
	streamNewDistributedMigrations $ \oldkey newkey ->
		unlessM (inAnnex newkey) $
			whenM (Command.ReKey.linkKey' oldkey newkey) $
				logStatus newkey InfoPresent
	next $ return True
