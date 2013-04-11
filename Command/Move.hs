{- git-annex command
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Common.Annex
import Command
import qualified Command.Drop
import qualified Annex
import Annex.Content
import qualified Remote
import Annex.UUID
import qualified Option
import Logs.Presence
import Logs.Transfer

def :: [Command]
def = [withOptions options $ command "move" paramPaths seek
	SectionCommon "move content of files to/from another repository"]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "source remote"

toOption :: Option
toOption = Option.field ['t'] "to" paramRemote "destination remote"

options :: [Option]
options = [fromOption, toOption]

seek :: [CommandSeek]
seek = [withField toOption Remote.byNameWithUUID $ \to ->
		withField fromOption Remote.byNameWithUUID $ \from ->
			withFilesInGit $ whenAnnexed $ start to from True]

start :: Maybe Remote -> Maybe Remote -> Bool -> FilePath -> (Key, Backend) -> CommandStart
start to from move file (key, _) = do
	noAuto
	case (from, to) of
		(Nothing, Nothing) -> error "specify either --from or --to"
		(Nothing, Just dest) -> toStart dest move file key
		(Just src, Nothing) -> fromStart src move file key
		(_ ,  _) -> error "only one of --from or --to can be specified"
  where
	noAuto = when move $ whenM (Annex.getState Annex.auto) $ error
		"--auto is not supported for move"

showMoveAction :: Bool -> FilePath -> Annex ()
showMoveAction True file = showStart "move" file
showMoveAction False file = showStart "copy" file

{- Moves (or copies) the content of an annexed file to a remote.
 -
 - If the remote already has the content, it is still removed from
 - the current repository.
 -
 - Note that unlike drop, this does not honor annex.numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
toStart :: Remote -> Bool -> FilePath -> Key -> CommandStart
toStart dest move file key = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else do
			showMoveAction move file
			next $ toPerform dest move key file
toPerform :: Remote -> Bool -> Key -> FilePath -> CommandPerform
toPerform dest move key file = moveLock move key $ do
	-- Checking the remote is expensive, so not done in the start step.
	-- In fast mode, location tracking is assumed to be correct,
	-- and an explicit check is not done, when copying. When moving,
	-- it has to be done, to avoid inaverdent data loss.
	fast <- Annex.getState Annex.fast
	let fastcheck = fast && not move && not (Remote.hasKeyCheap dest)
	isthere <- if fastcheck
		then Right <$> expectedpresent
		else Remote.hasKey dest key
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> do
			showAction $ "to " ++ Remote.name dest
			ok <- upload (Remote.uuid dest) key (Just file) noRetry $
				Remote.storeKey dest key (Just file)
			if ok
				then do
					Remote.logStatus dest key InfoPresent
					finish
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> do
			unlessM expectedpresent $
				Remote.logStatus dest key InfoPresent
			finish
  where
	finish
		| move = do
			removeAnnex key
			next $ Command.Drop.cleanupLocal key
		| otherwise = next $ return True
	expectedpresent = do
		remotes <- Remote.keyPossibilities key
		return $ dest `elem` remotes

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: Remote -> Bool -> FilePath -> Key -> CommandStart
fromStart src move file key
	| move = go
	| otherwise = stopUnless (not <$> inAnnex key) go
  where
	go = stopUnless (fromOk src key) $ do
		showMoveAction move file
		next $ fromPerform src move key file

fromOk :: Remote -> Key -> Annex Bool
fromOk src key
	| Remote.hasKeyCheap src =
		either (const expensive) return =<< Remote.hasKey src key
	| otherwise = expensive
  where
	expensive = do
		u <- getUUID
		remotes <- Remote.keyPossibilities key
		return $ u /= Remote.uuid src && elem src remotes

fromPerform :: Remote -> Bool -> Key -> FilePath -> CommandPerform
fromPerform src move key file = moveLock move key $
	ifM (inAnnex key)
		( handle move True
		, handle move =<< go
		)
  where
	go = download (Remote.uuid src) key (Just file) noRetry $ \p -> do
		showAction $ "from " ++ Remote.name src
		getViaTmp key $ \t -> Remote.retrieveKeyFile src key (Just file) t p
	handle _ False = stop -- failed
	handle False True = next $ return True -- copy complete
	handle True True = do -- finish moving
		ok <- Remote.removeKey src key
		next $ Command.Drop.cleanupRemote key src ok

{- Locks a key in order for it to be moved.
 - No lock is needed when a key is being copied. -}
moveLock :: Bool -> Key -> Annex a -> Annex a
moveLock True key a = lockContent key a
moveLock False _ a = a
