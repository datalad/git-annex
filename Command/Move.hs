{- git-annex command
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
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
import Annex.Transfer
import Logs.Presence

def :: [Command]
def = [withOptions moveOptions $ command "move" paramPaths seek
	SectionCommon "move content of files to/from another repository"]

moveOptions :: [Option]
moveOptions = fromToOptions ++ keyOptions

seek :: CommandSeek
seek ps = do
	to <- getOptionField toOption Remote.byNameWithUUID
	from <- getOptionField fromOption Remote.byNameWithUUID
	withKeyOptions
		(startKey to from True)
		(withFilesInGit $ whenAnnexed $ start to from True)
		ps

start :: Maybe Remote -> Maybe Remote -> Bool -> FilePath -> Key -> CommandStart
start to from move file key = start' to from move (Just file) key

startKey :: Maybe Remote -> Maybe Remote -> Bool -> Key -> CommandStart
startKey to from move = start' to from move Nothing

start' :: Maybe Remote -> Maybe Remote -> Bool -> AssociatedFile -> Key -> CommandStart
start' to from move afile key = do
	noAuto
	case (from, to) of
		(Nothing, Nothing) -> error "specify either --from or --to"
		(Nothing, Just dest) -> toStart dest move afile key
		(Just src, Nothing) -> fromStart src move afile key
		_ -> error "only one of --from or --to can be specified"
  where
	noAuto = when move $ whenM (Annex.getState Annex.auto) $ error
		"--auto is not supported for move"

showMoveAction :: Bool -> Key -> AssociatedFile -> Annex ()
showMoveAction move = showStart' (if move then "move" else "copy")

{- Moves (or copies) the content of an annexed file to a remote.
 -
 - If the remote already has the content, it is still removed from
 - the current repository.
 -
 - Note that unlike drop, this does not honor numcopies.
 - A file's content can be moved even if there are insufficient copies to
 - allow it to be dropped.
 -}
toStart :: Remote -> Bool -> AssociatedFile -> Key -> CommandStart
toStart dest move afile key = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else toStart' dest move afile key

toStart' :: Remote -> Bool -> AssociatedFile -> Key -> CommandStart
toStart' dest move afile key = do
	fast <- Annex.getState Annex.fast
	if fast && not move && not (Remote.hasKeyCheap dest)
		then ifM (expectedPresent dest key)
			( stop
			, go True (pure $ Right False)
			)
		else go False (Remote.hasKey dest key)
  where
	go fastcheck isthere = do
		showMoveAction move key afile
		next $ toPerform dest move key afile fastcheck =<< isthere

expectedPresent :: Remote -> Key -> Annex Bool
expectedPresent dest key = do
	remotes <- Remote.keyPossibilities key
	return $ dest `elem` remotes

toPerform :: Remote -> Bool -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform dest move key afile fastcheck isthere = moveLock move key $
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> do
			showAction $ "to " ++ Remote.name dest
			ok <- notifyTransfer Upload afile $
				upload (Remote.uuid dest) key afile noRetry $
					Remote.storeKey dest key afile
			if ok
				then do
					Remote.logStatus dest key InfoPresent
					finish
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> do
			unlessM (expectedPresent dest key) $
				Remote.logStatus dest key InfoPresent
			finish
  where
	finish
		| move = do
			removeAnnex key
			next $ Command.Drop.cleanupLocal key
		| otherwise = next $ return True

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: Remote -> Bool -> AssociatedFile -> Key -> CommandStart
fromStart src move afile key
	| move = go
	| otherwise = stopUnless (not <$> inAnnex key) go
  where
	go = stopUnless (fromOk src key) $ do
		showMoveAction move key afile
		next $ fromPerform src move key afile

fromOk :: Remote -> Key -> Annex Bool
fromOk src key = go =<< Annex.getState Annex.force
  where
	go True = either (const $ return True) return =<< haskey
	go False
		| Remote.hasKeyCheap src =
			either (const expensive) return =<< haskey
		| otherwise = expensive
	haskey = Remote.hasKey src key
	expensive = do
		u <- getUUID
		remotes <- Remote.keyPossibilities key
		return $ u /= Remote.uuid src && elem src remotes

fromPerform :: Remote -> Bool -> Key -> AssociatedFile -> CommandPerform
fromPerform src move key afile = moveLock move key $
	ifM (inAnnex key)
		( handle move True
		, handle move =<< go
		)
  where
	go = notifyTransfer Download afile $ 
		download (Remote.uuid src) key afile noRetry $ \p -> do
			showAction $ "from " ++ Remote.name src
			getViaTmp key $ \t -> Remote.retrieveKeyFile src key afile t p
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
