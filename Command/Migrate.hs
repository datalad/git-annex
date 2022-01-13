{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
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
import Logs.MetaData
import Logs.Web
import Utility.Metered

cmd :: Command
cmd = withGlobalOptions [annexedMatchingOptions] $
	command "migrate" SectionUtility 
		"switch data to different backend"
		paramPaths (seek <$$> optParser)

data MigrateOptions = MigrateOptions
	{ migrateThese :: CmdParams
	, removeSize :: Bool
	}

optParser :: CmdParamsDesc -> Parser MigrateOptions
optParser desc = MigrateOptions
	<$> cmdParams desc
	<*> switch
		( long "remove-size"
		<> help "remove size field from keys"
		)

seek :: MigrateOptions -> CommandSeek
seek o = withFilesInGitAnnex ww seeker =<< workTreeItems ww (migrateThese o)
  where
	ww = WarnUnmatchLsFiles
	seeker = AnnexedFileSeeker
		{ startAction = start o
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}

start :: MigrateOptions -> SeekInput -> RawFilePath -> Key -> CommandStart
start o si file key = do
	forced <- Annex.getState Annex.force
	v <- Backend.getBackend (fromRawFilePath file) key
	case v of
		Nothing -> stop
		Just oldbackend -> do
			exists <- inAnnex key
			newbackend <- maybe defaultBackend return 
				=<< chooseBackend file
			if (newbackend /= oldbackend || upgradableKey oldbackend key || forced) && exists
				then go False oldbackend newbackend
				else if removeSize o && exists
					then go True oldbackend oldbackend
					else stop
  where
	go onlyremovesize oldbackend newbackend =
		starting "migrate" (mkActionItem (key, file)) si $
			perform onlyremovesize o file key oldbackend newbackend

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
perform :: Bool -> MigrateOptions -> RawFilePath -> Key -> Backend -> Backend -> CommandPerform
perform onlyremovesize o file oldkey oldbackend newbackend = go =<< genkey (fastMigrate oldbackend)
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
			next $ Command.ReKey.cleanup file newkey
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
		newkey <- fst <$> genKey source nullMeterUpdate (Just newbackend)
		return $ Just (newkey, False)
	genkey (Just fm) = fm oldkey newbackend afile >>= \case
		Just newkey -> return (Just (newkey, True))
		Nothing -> genkey Nothing
	removesize k
		| removeSize o = alterKey k $ \kd -> kd { keySize = Nothing } 
		| otherwise = k
	afile = AssociatedFile (Just file)
