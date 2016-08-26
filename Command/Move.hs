{- git-annex command
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Move where

import Command
import qualified Command.Drop
import qualified Annex
import Annex.Content
import qualified Remote
import Annex.UUID
import Annex.Transfer
import Logs.Presence
import Annex.NumCopies

import System.Log.Logger (debugM)

cmd :: Command
cmd = withGlobalOptions (jobsOption : annexedMatchingOptions) $
	command "move" SectionCommon
		"move content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MoveOptions = MoveOptions
	{ moveFiles :: CmdParams
	, fromToOptions :: FromToOptions
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser MoveOptions
optParser desc = MoveOptions
	<$> cmdParams desc
	<*> parseFromToOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)

instance DeferredParseClass MoveOptions where
	finishParse v = MoveOptions
		<$> pure (moveFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (keyOptions v)

seek :: MoveOptions -> CommandSeek
seek o = allowConcurrentOutput $ 
	withKeyOptions (keyOptions o) False
		(startKey o True)
		(withFilesInGit $ whenAnnexed $ start o True)
		(moveFiles o)

start :: MoveOptions -> Bool -> FilePath -> Key -> CommandStart
start o move f k = start' o move afile k (mkActionItem afile)
  where
	afile = Just f

startKey :: MoveOptions -> Bool -> Key -> ActionItem -> CommandStart
startKey o move = start' o move Nothing

start' :: MoveOptions -> Bool -> AssociatedFile -> Key -> ActionItem -> CommandStart
start' o move afile key ai = 
	case fromToOptions o of
		FromRemote src -> checkFailedTransferDirection ai Download $
			fromStart move afile key ai =<< getParsed src
		ToRemote dest -> checkFailedTransferDirection ai Upload $
			toStart move afile key ai =<< getParsed dest

showMoveAction :: Bool -> Key -> ActionItem -> Annex ()
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
toStart :: Bool -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
toStart move afile key ai dest = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else toStart' dest move afile key ai

toStart' :: Remote -> Bool -> AssociatedFile -> Key -> ActionItem -> CommandStart
toStart' dest move afile key ai = do
	fast <- Annex.getState Annex.fast
	if fast && not move && not (Remote.hasKeyCheap dest)
		then ifM (expectedPresent dest key)
			( stop
			, go True (pure $ Right False)
			)
		else go False (Remote.hasKey dest key)
  where
	go fastcheck isthere = do
		showMoveAction move key ai
		next $ toPerform dest move key afile fastcheck =<< isthere

expectedPresent :: Remote -> Key -> Annex Bool
expectedPresent dest key = do
	remotes <- Remote.keyPossibilities key
	return $ dest `elem` remotes

toPerform :: Remote -> Bool -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform dest move key afile fastcheck isthere =
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
				then finish $
					Remote.logStatus dest key InfoPresent
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> finish $
			unlessM (expectedPresent dest key) $
				Remote.logStatus dest key InfoPresent
  where
	finish :: Annex () -> CommandPerform
	finish setpresentremote
		| move = lockContentForRemoval key $ \contentlock -> do
			-- Drop content before updating location logs,
			-- in case disk space is very low this frees up
			-- space before writing data to disk.
			removeAnnex contentlock
			next $ do
				setpresentremote
				Command.Drop.cleanupLocal key
		| otherwise = next $ do
			setpresentremote
			return True

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: Bool -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
fromStart move afile key ai src
	| move = go
	| otherwise = stopUnless (not <$> inAnnex key) go
  where
	go = stopUnless (fromOk src key) $ do
		showMoveAction move key ai
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
fromPerform src move key afile = ifM (inAnnex key)
	( dispatch move True
	, dispatch move =<< go
	)
  where
	go = notifyTransfer Download afile $ 
		download (Remote.uuid src) key afile noRetry $ \p -> do
			showAction $ "from " ++ Remote.name src
			getViaTmp (RemoteVerify src) key $ \t ->
				Remote.retrieveKeyFile src key afile t p
	dispatch _ False = stop -- failed
	dispatch False True = next $ return True -- copy complete
	-- Finish by dropping from remote, taking care to verify that
	-- the copy here has not been lost somehow. 
	-- (NumCopies is 1 since we're moving.)
	dispatch True True = verifyEnoughCopiesToDrop "" key Nothing
		(NumCopies 1) [] [] [UnVerifiedHere] dropremote faileddropremote
	dropremote proof = do
		liftIO $ debugM "drop" $ unwords
			[ "Dropping from remote"
			, show src
			, "proof:"
			, show proof
			]
		ok <- Remote.removeKey src key
		next $ Command.Drop.cleanupRemote key src ok
	faileddropremote = error "Unable to drop from remote."
