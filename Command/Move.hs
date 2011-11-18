{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
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

def :: [Command]
def = [dontCheck toOpt $ dontCheck fromOpt $
	command "move" paramPaths seek
	"move content of files to/from another repository"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed $ start True]

{- Move (or copy) a file either --to or --from a repository.
 -
 - This only operates on the cached file content; it does not involve
 - moving data in the key-value backend. -}
start :: Bool -> FilePath -> (Key, Backend Annex) -> CommandStart
start move file (key, _) = do
	noAuto
	to <- Annex.getState Annex.toremote
	from <- Annex.getState Annex.fromremote
	case (from, to) of
		(Nothing, Nothing) -> error "specify either --from or --to"
		(Nothing, Just name) -> do
			dest <- Remote.byName name
			toStart dest move file key
		(Just name, Nothing) -> do
			src <- Remote.byName name
			fromStart src move file key
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
toStart :: Remote.Remote Annex -> Bool -> FilePath -> Key -> CommandStart
toStart dest move file key = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else do
			showMoveAction move file
			next $ toPerform dest move key
toPerform :: Remote.Remote Annex -> Bool -> Key -> CommandPerform
toPerform dest move key = moveLock move key $ do
	-- Checking the remote is expensive, so not done in the start step.
	-- In fast mode, location tracking is assumed to be correct,
	-- and an explicit check is not done, when copying. When moving,
	-- it has to be done, to avoid inaverdent data loss.
	fast <- Annex.getState Annex.fast
	let fastcheck = fast && not move && not (Remote.hasKeyCheap dest)
	isthere <- if fastcheck
		then do
			remotes <- Remote.keyPossibilities key
			return $ Right $ dest `elem` remotes
		else Remote.hasKey dest key
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> do
			showAction $ "to " ++ Remote.name dest
			ok <- Remote.storeKey dest key
			if ok
				then finish
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> finish
	where
		finish = do
			Remote.logStatus dest key True
			if move
				then do
					whenM (inAnnex key) $ removeAnnex key
					next $ Command.Drop.cleanupLocal key
				else next $ return True

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: Remote.Remote Annex -> Bool -> FilePath -> Key -> CommandStart
fromStart src move file key
	| move = go
	| otherwise = do
		ishere <- inAnnex key
		if ishere then stop else go
	where
		go = do
			ok <- fromOk src key
			if ok
				then do
					showMoveAction move file
					next $ fromPerform src move key
				else stop
fromOk :: Remote.Remote Annex -> Key -> Annex Bool
fromOk src key = do
	u <- getUUID
	remotes <- Remote.keyPossibilities key
	return $ u /= Remote.uuid src && any (== src) remotes
fromPerform :: Remote.Remote Annex -> Bool -> Key -> CommandPerform
fromPerform src move key = moveLock move key $ do
	ishere <- inAnnex key
	if ishere
		then handle move True
		else do
			showAction $ "from " ++ Remote.name src
			ok <- getViaTmp key $ Remote.retrieveKeyFile src key
			handle move ok
	where
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
