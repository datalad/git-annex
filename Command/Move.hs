{- git-annex command
 -
 - Copyright 2010-2018 Joey Hess <id@joeyh.name>
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
cmd = withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "move" SectionCommon
		"move content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MoveOptions = MoveOptions
	{ moveFiles :: CmdParams
	, fromToOptions :: FromToHereOptions
	, removeWhenOptions :: RemoveWhen
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser MoveOptions
optParser desc = MoveOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> parseRemoveWhenOptions
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseBatchOption

instance DeferredParseClass MoveOptions where
	finishParse v = MoveOptions
		<$> pure (moveFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (removeWhenOptions v)
		<*> pure (keyOptions v)
		<*> pure (batchOption v)

data RemoveWhen = RemoveSafe | RemoveUnsafe | RemoveNever
	deriving (Show, Eq)

parseRemoveWhenOptions :: Parser RemoveWhen
parseRemoveWhenOptions = 
	flag' RemoveSafe
		( long "safe"
		<> short 's'
		<> help "preserve numcopies"
		)
	<|> flag' RemoveUnsafe
		(long "unsafe"
		<> short 'u'
		<> help "do not preserve numcopies (default)"
		)
	<|> pure RemoveUnsafe

seek :: MoveOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	let go = whenAnnexed $ start (fromToOptions o) (removeWhenOptions o)
	case batchOption o of
		Batch -> batchInput Right (batchCommandAction . go)
		NoBatch -> withKeyOptions (keyOptions o) False
			(startKey (fromToOptions o) (removeWhenOptions o))
			(withFilesInGit go)
			=<< workTreeItems (moveFiles o)

start :: FromToHereOptions -> RemoveWhen -> FilePath -> Key -> CommandStart
start fromto removewhen f k =
	start' fromto removewhen afile k (mkActionItem afile)
  where
	afile = AssociatedFile (Just f)

startKey :: FromToHereOptions -> RemoveWhen -> Key -> ActionItem -> CommandStart
startKey fromto removewhen = start' fromto removewhen (AssociatedFile Nothing)

start' :: FromToHereOptions -> RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
start' fromto removewhen afile key ai = onlyActionOn key $
	case fromto of
		Right (FromRemote src) ->
			checkFailedTransferDirection ai Download $
				fromStart removewhen afile key ai =<< getParsed src
		Right (ToRemote dest) ->
			checkFailedTransferDirection ai Upload $
				toStart removewhen afile key ai =<< getParsed dest
		Left ToHere ->
			checkFailedTransferDirection ai Download $
				toHereStart removewhen afile key ai

showMoveAction :: RemoveWhen -> Key -> ActionItem -> Annex ()
showMoveAction RemoveNever = showStartKey "copy"
showMoveAction _ = showStartKey "move"

toStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
toStart removewhen afile key ai dest = do
	u <- getUUID
	ishere <- inAnnex key
	if not ishere || u == Remote.uuid dest
		then stop -- not here, so nothing to do
		else toStart' dest removewhen afile key ai

toStart' :: Remote -> RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
toStart' dest removewhen afile key ai = do
	fast <- Annex.getState Annex.fast
	if fast && removewhen == RemoveNever
		then ifM (expectedPresent dest key)
			( stop
			, go True (pure $ Right False)
			)
		else go False (Remote.hasKey dest key)
  where
	go fastcheck isthere = do
		showMoveAction removewhen key ai
		next $ toPerform dest removewhen key afile fastcheck =<< isthere

expectedPresent :: Remote -> Key -> Annex Bool
expectedPresent dest key = do
	remotes <- Remote.keyPossibilities key
	return $ dest `elem` remotes

toPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform dest removewhen key afile fastcheck isthere =
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> do
			showAction $ "to " ++ Remote.name dest
			ok <- notifyTransfer Upload afile $
				upload (Remote.uuid dest) key afile stdRetry $
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
	finish setpresentremote = case removewhen of
		RemoveNever -> do
			setpresentremote
			next $ return True
		_ -> lockContentForRemoval key $ \contentlock -> do
			numcopies <- case removewhen of
				RemoveUnsafe -> pure (NumCopies 1)
				_ -> getAssociatedFileNumCopies afile
			u <- getUUID
			let drophere proof = do
				liftIO $ debugM "drop" $ unwords
					[ "Dropping from here"
					, "proof:"
					, show proof
					]
				-- Drop content before updating location logs,
				-- in case disk space is very low this frees
				-- up space before writing data to disk.
				removeAnnex contentlock
				next $ do
					setpresentremote
					Command.Drop.cleanupLocal key
			let faileddrophere = do
				warning "Not enough copies exist to drop from here (use --unsafe to avoid this check)"
				next $ do
					setpresentremote
					return True
			(tocheck, verified) <- verifiableCopies key [u]
			verifyEnoughCopiesToDrop "" key (Just contentlock)
				 numcopies [] verified
				 (UnVerifiedRemote dest : tocheck)
				 drophere faileddrophere

{- Moves (or copies) the content of an annexed file from a remote
 - to the current repository.
 -
 - If the current repository already has the content, it is still removed
 - from the remote.
 -}
fromStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> Remote -> CommandStart
fromStart removewhen afile key ai src
	| removewhen == RemoveNever = stopUnless (not <$> inAnnex key) go
	| otherwise = go
  where
	go = stopUnless (fromOk src key) $ do
		showMoveAction removewhen key ai
		next $ fromPerform src removewhen key afile

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

fromPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> CommandPerform
fromPerform src removewhen key afile = do
	showAction $ "from " ++ Remote.name src
	ifM (inAnnex key)
		( dispatch removewhen True
		, dispatch removewhen =<< go
		)
  where
	go = notifyTransfer Download afile $ 
		download (Remote.uuid src) key afile stdRetry $ \p ->
			getViaTmp (RemoteVerify src) key $ \t ->
				Remote.retrieveKeyFile src key afile t p
	dispatch _ False = stop -- failed
	dispatch RemoveNever True = next $ return True -- copy complete
	-- Finish move by dropping from remote, when verified 
	-- numcopies or RemoveUnsafe allows.
	dispatch _ True = do
		numcopies <- case removewhen of
			RemoveUnsafe -> pure (NumCopies 1)
			_ -> getAssociatedFileNumCopies afile
		(tocheck, verified) <- verifiableCopies key [Remote.uuid src]
		verifyEnoughCopiesToDrop "" key Nothing numcopies [] verified
			tocheck dropremote faileddropremote
	dropremote proof = do
		liftIO $ debugM "drop" $ unwords
			[ "Dropping from remote"
			, show src
			, "proof:"
			, show proof
			]
		ok <- Remote.removeKey src key
		next $ Command.Drop.cleanupRemote key src ok
	faileddropremote = case removewhen of
		RemoveUnsafe -> giveup "Unable to drop from remote."
		RemoveSafe -> do
			warning "Not enough copies exist to drop from remote (use --unsafe to avoid this check)"
			next $ return True
		RemoveNever -> next $ return True

{- Moves (or copies) the content of an annexed file from reachable remotes
 - to the current repository.
 -
 - When moving, the content is removed from all the reachable remotes. -}
toHereStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> CommandStart
toHereStart removewhen afile key ai
	| removewhen == RemoveNever = stopUnless (not <$> inAnnex key) go
	| otherwise = go
  where
	go = do
		rs <- Remote.keyPossibilities key
		forM_ rs $ \r ->
			includeCommandAction $ do
				showMoveAction removewhen key ai
				next $ fromPerform r removewhen key afile
		stop
