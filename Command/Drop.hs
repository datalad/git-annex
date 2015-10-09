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

import System.Log.Logger (debugM)
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
				Nothing -> startLocal afile numcopies key []
				Just remote -> do
					u <- getUUID
					if Remote.uuid remote == u
						then startLocal afile numcopies key []
						else startRemote afile numcopies key remote
	  where
		want from
			| autoMode o = wantDrop False (Remote.uuid <$> from) (Just key) afile
			| otherwise = return True

startKeys :: DropOptions -> Key -> CommandStart
startKeys o key = start' o key Nothing

startLocal :: AssociatedFile -> NumCopies -> Key -> [VerifiedCopy] -> CommandStart
startLocal afile numcopies key preverified = stopUnless (inAnnex key) $ do
	showStart' "drop" key afile
	next $ performLocal key afile numcopies preverified

startRemote :: AssociatedFile -> NumCopies -> Key -> Remote -> CommandStart
startRemote afile numcopies key remote = do
	showStart' ("drop " ++ Remote.name remote) key afile
	next $ performRemote key afile numcopies remote

performLocal :: Key -> AssociatedFile -> NumCopies -> [VerifiedCopy] -> CommandPerform
performLocal key afile numcopies preverified = lockContentForRemoval key $ \contentlock -> do
	u <- getUUID
	(tocheck, verified) <- verifiableCopies key [u]
	doDrop u (Just contentlock) key afile numcopies [] (preverified ++ verified) tocheck
		( \proof -> do
			liftIO $ debugM "drop" $ unwords
				[ "Dropping from here"
				, "proof:"
				, show proof
				]
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
	(tocheck, verified) <- verifiableCopies key [uuid]
	doDrop uuid Nothing key afile numcopies [uuid] verified tocheck
		( \proof -> do 
			liftIO $ debugM "drop" $ unwords
				[ "Dropping from remote"
				, show remote
				, "proof: "
				, show proof
				]
			ok <- Remote.removeKey remote key
			next $ cleanupRemote key remote ok
		, stop
		)
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

{- Before running the dropaction, checks specified remotes to
 - verify that enough copies of a key exist to allow it to be
 - safely removed (with no data loss).
 -
 - Also checks if it's required content, and refuses to drop if so.
 -
 - --force overrides and always allows dropping.
 -}
doDrop
	:: UUID
	-> Maybe ContentRemovalLock
	-> Key
	-> AssociatedFile
	-> NumCopies
	-> [UUID]
	-> [VerifiedCopy]
	-> [UnVerifiedCopy]
	-> (Maybe SafeDropProof -> CommandPerform, CommandPerform)
	-> CommandPerform
doDrop dropfrom contentlock key afile numcopies skip preverified check (dropaction, nodropaction) = 
	ifM (Annex.getState Annex.force)
		( dropaction Nothing
		, ifM (checkRequiredContent dropfrom key afile)
			( verifyEnoughCopiesToDrop nolocmsg key 
				contentlock numcopies
				skip preverified check
					(dropaction . Just)
					(forcehint nodropaction)
			, stop
			)
		)
  where
	nolocmsg = "Rather than dropping this file, try using: git annex move"
	forcehint a = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		a

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
