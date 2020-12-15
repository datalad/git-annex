{- git-annex command
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Logs.Trust
import Logs.File
import Annex.NumCopies

import System.Log.Logger (debugM)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "move" SectionCommon
		"move content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MoveOptions = MoveOptions
	{ moveFiles :: CmdParams
	, fromToOptions :: FromToHereOptions
	, removeWhen :: RemoveWhen
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser MoveOptions
optParser desc = MoveOptions
	<$> cmdParams desc
	<*> parseFromToHereOptions
	<*> pure RemoveSafe
	<*> optional (parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseBatchOption

instance DeferredParseClass MoveOptions where
	finishParse v = MoveOptions
		<$> pure (moveFiles v)
		<*> finishParse (fromToOptions v)
		<*> pure (removeWhen v)
		<*> pure (keyOptions v)
		<*> pure (batchOption v)

data RemoveWhen = RemoveSafe | RemoveNever
	deriving (Show, Eq)

seek :: MoveOptions -> CommandSeek
seek o = startConcurrency stages $ do
	case batchOption o of
		NoBatch -> withKeyOptions (keyOptions o) False seeker
			(commandAction . startKey (fromToOptions o) (removeWhen o))
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (moveFiles o)
		Batch fmt -> batchAnnexedFilesMatching fmt seeker
  where
	seeker = AnnexedFileSeeker
		{ startAction = start (fromToOptions o) (removeWhen o)
		, checkContentPresent = case fromToOptions o of
			Right (FromRemote _) -> Nothing
			Right (ToRemote _) -> Just True
			Left ToHere -> Nothing
		, usesLocationLog = True
		}
	stages = case fromToOptions o of
		Right (FromRemote _) -> downloadStages
		Right (ToRemote _) -> commandStages
		Left ToHere -> downloadStages
	ww = WarnUnmatchLsFiles

start :: FromToHereOptions -> RemoveWhen -> SeekInput -> RawFilePath -> Key -> CommandStart
start fromto removewhen si f k = start' fromto removewhen afile si k ai
  where
	afile = AssociatedFile (Just f)
	ai = mkActionItem (k, afile)

startKey :: FromToHereOptions -> RemoveWhen -> (SeekInput, Key, ActionItem) -> CommandStart
startKey fromto removewhen (si, k, ai) = 
	start' fromto removewhen (AssociatedFile Nothing) si k ai

start' :: FromToHereOptions -> RemoveWhen -> AssociatedFile -> SeekInput -> Key -> ActionItem -> CommandStart
start' fromto removewhen afile si key ai =
	case fromto of
		Right (FromRemote src) ->
			checkFailedTransferDirection ai Download $
				fromStart removewhen afile key ai si =<< getParsed src
		Right (ToRemote dest) ->
			checkFailedTransferDirection ai Upload $
				toStart removewhen afile key ai si =<< getParsed dest
		Left ToHere ->
			checkFailedTransferDirection ai Download $
				toHereStart removewhen afile key ai si

describeMoveAction :: RemoveWhen -> String
describeMoveAction RemoveNever = "copy"
describeMoveAction _ = "move"

toStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> SeekInput -> Remote -> CommandStart
toStart removewhen afile key ai si dest = do
	u <- getUUID
	if u == Remote.uuid dest
		then stop
		else toStart' dest removewhen afile key ai si

toStart' :: Remote -> RemoveWhen -> AssociatedFile -> Key -> ActionItem -> SeekInput -> CommandStart
toStart' dest removewhen afile key ai si = do
	fast <- Annex.getState Annex.fast
	if fast && removewhen == RemoveNever
		then ifM (expectedPresent dest key)
			( stop
			, go True (pure $ Right False)
			)
		else go False (Remote.hasKey dest key)
  where
	go fastcheck isthere =
		starting (describeMoveAction removewhen) (OnlyActionOn key ai) si $
			toPerform dest removewhen key afile fastcheck =<< isthere

expectedPresent :: Remote -> Key -> Annex Bool
expectedPresent dest key = do
	remotes <- Remote.keyPossibilities key
	return $ dest `elem` remotes

toPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform dest removewhen key afile fastcheck isthere = do
	srcuuid <- getUUID
	case isthere of
		Left err -> do
			showNote err
			stop
		Right False -> logMove srcuuid destuuid False key $ \deststartedwithcopy -> do
			showAction $ "to " ++ Remote.name dest
			ok <- notifyTransfer Upload afile $
				upload dest key afile stdRetry
			if ok
				then finish deststartedwithcopy $
					Remote.logStatus dest key InfoPresent
				else do
					when fastcheck $
						warning "This could have failed because --fast is enabled."
					stop
		Right True -> logMove srcuuid destuuid True key $ \deststartedwithcopy ->
			finish deststartedwithcopy $
				unlessM (expectedPresent dest key) $
					Remote.logStatus dest key InfoPresent
  where
	destuuid = Remote.uuid dest
	finish deststartedwithcopy setpresentremote = case removewhen of
		RemoveNever -> do
			setpresentremote
			next $ return True
		RemoveSafe -> lockContentForRemoval key lockfailed $ \contentlock -> do
			srcuuid <- getUUID
			willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile >>= \case
				DropAllowed -> drophere setpresentremote contentlock "moved"
				DropCheckNumCopies -> do
					numcopies <- getAssociatedFileNumCopies afile
					(tocheck, verified) <- verifiableCopies key [srcuuid]
					verifyEnoughCopiesToDrop "" key (Just contentlock)
						 numcopies [srcuuid] verified
						 (UnVerifiedRemote dest : tocheck)
						 (drophere setpresentremote contentlock . showproof)
						 (faileddrophere setpresentremote)
				DropWorse -> faileddrophere setpresentremote
	showproof proof = "proof: " ++ show proof
	drophere setpresentremote contentlock reason = do
		liftIO $ debugM "move" $ unwords
			[ "Dropping from here"
			, "(" ++ reason ++ ")"
			]
		-- Drop content before updating location logs,
		-- in case disk space is very low this frees
		-- up space before writing data to disk.
		removeAnnex contentlock
		next $ do
			() <- setpresentremote
			Command.Drop.cleanupLocal key
	faileddrophere setpresentremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote "Content not dropped from here."
		next $ do
			() <- setpresentremote
			return False
	
	-- This occurs when, for example, two files are being dropped
	-- and have the same content. The seek stage checks if the content
	-- is present, but due to buffering, may find it present for the
	-- second file before the first is dropped. If so, nothing remains
	-- to be done except for cleaning up.
	lockfailed = next $ Command.Drop.cleanupLocal key

fromStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> SeekInput -> Remote -> CommandStart
fromStart removewhen afile key ai si src = 
	stopUnless (fromOk src key) $
		starting (describeMoveAction removewhen) (OnlyActionOn key ai) si $
			fromPerform src removewhen key afile

fromOk :: Remote -> Key -> Annex Bool
fromOk src key
	-- check if the remote contains the key, when it can be done cheaply
	| Remote.hasKeyCheap src = 
		Remote.hasKey src key >>= \case
			Right True -> return True
			-- Don't skip getting the key just because the
			-- remote no longer contains it if the log
			-- says the remote is supposed to contain it;
			-- that would be surprising behavior.
			_ -> checklog
	| otherwise = checklog
  where
	checklog = do
		u <- getUUID
		remotes <- Remote.keyPossibilities key
		return $ u /= Remote.uuid src && elem src remotes

fromPerform :: Remote -> RemoveWhen -> Key -> AssociatedFile -> CommandPerform
fromPerform src removewhen key afile = do
	showAction $ "from " ++ Remote.name src
	present <- inAnnex key
	destuuid <- getUUID
	logMove srcuuid destuuid present key $ \deststartedwithcopy ->
		if present
			then dispatch removewhen deststartedwithcopy True
			else dispatch removewhen deststartedwithcopy =<< get
  where
	get = notifyTransfer Download afile $
		download src key afile stdRetry
	
	dispatch _ _ False = stop -- failed
	dispatch RemoveNever _ True = next $ return True -- copy complete
	dispatch RemoveSafe deststartedwithcopy True = lockContentShared key $ \_lck -> do
		destuuid <- getUUID
		willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile >>= \case
			DropAllowed -> dropremote "moved"
			DropCheckNumCopies -> do
				numcopies <- getAssociatedFileNumCopies afile
				(tocheck, verified) <- verifiableCopies key [Remote.uuid src]
				verifyEnoughCopiesToDrop "" key Nothing numcopies [Remote.uuid src] verified
					tocheck (dropremote . showproof) faileddropremote
			DropWorse -> faileddropremote		
	
	srcuuid = Remote.uuid src
	
	showproof proof = "proof: " ++ show proof
	
	dropremote reason = do
		liftIO $ debugM "move" $ unwords
			[ "Dropping from remote"
			, show src
			, "(" ++ reason ++ ")"
			]
		ok <- Remote.action (Remote.removeKey src key)
		next $ Command.Drop.cleanupRemote key src ok
	
	faileddropremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote $ "Content not dropped from " ++ Remote.name src ++ "."
		next $ return False

{- Moves (or copies) the content of an annexed file from reachable remotes
 - to the current repository.
 -
 - When moving, the content is removed from all the reachable remotes that
 - it can safely be removed from. -}
toHereStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> SeekInput -> CommandStart
toHereStart removewhen afile key ai si = 
	startingNoMessage (OnlyActionOn key ai) $ do
		rs <- Remote.keyPossibilities key
		forM_ rs $ \r ->
			includeCommandAction $
				starting (describeMoveAction removewhen) ai si $
					fromPerform r removewhen key afile
		next $ return True

{- The goal of this command is to allow the user maximum freedom to move
 - files as they like, while avoiding making bad situations any worse
 - than they already were.
 -
 - When the destination repository already had a copy of a file
 - before the move operation began, dropping it from the source
 - repository reduces the number of copies, and should fail if
 - that would violate numcopies settings.
 -
 - On the other hand, when the destiation repository does not already
 - have a copy of a file, it can be dropped without making numcopies
 - worse, so the move is allowed even if numcopies is not met.
 -
 - Similarly, a file can move from an untrusted repository to another
 - untrusted repository, even if that is the only copy of the file.
 -
 - But, moving a file from a repository with higher trust to an untrusted
 - repository must still check that there are enough other copies to be
 - safe.
 -
 - Also, required content settings should not be violated.
 -
 - This function checks all that. It needs to know if the destination
 - repository already had a copy of the file before the move began.
 -}
willDropMakeItWorse :: UUID -> UUID -> DestStartedWithCopy -> Key -> AssociatedFile -> Annex DropCheck
willDropMakeItWorse srcuuid destuuid (DestStartedWithCopy deststartedwithcopy) key afile =
	ifM (Command.Drop.checkRequiredContent srcuuid key afile)
		( if deststartedwithcopy
			then unlessforced DropCheckNumCopies
			else ifM checktrustlevel
				( return DropAllowed
				, unlessforced DropCheckNumCopies
				)
		, unlessforced DropWorse
		)
  where
	unlessforced r = ifM (Annex.getState Annex.force)
		( return DropAllowed
		, return r
		)
	checktrustlevel = do
		desttrust <- lookupTrust destuuid
		srctrust <- lookupTrust srcuuid
		return (desttrust > UnTrusted || desttrust >= srctrust)

data DropCheck = DropWorse | DropAllowed | DropCheckNumCopies

newtype DestStartedWithCopy = DestStartedWithCopy Bool

{- Runs an action that performs a move, and logs the move, allowing an
 - interrupted move to be restarted later.
 -
 - This deals with the situation where dest did not start with a copy,
 - but the move downloaded it, and was then interrupted before dropping
 - it from the source. Re-running the move would see dest has a
 - copy, and so could refuse to allow the drop. By providing the logged
 - DestStartedWithCopy, this avoids that annoyance.
 -}
logMove :: UUID -> UUID -> Bool -> Key -> (DestStartedWithCopy -> Annex a) -> Annex a
logMove srcuuid destuuid deststartedwithcopy key a = bracket setup cleanup go
  where
	logline = L.fromStrict $ B8.unwords
		[ fromUUID srcuuid
		, fromUUID destuuid
		, serializeKey' key
		]

	setup = do
		logf <- fromRepo gitAnnexMoveLog
		-- Only log when there was no copy.
		unless deststartedwithcopy $
			appendLogFile logf gitAnnexMoveLock logline
		return logf

	cleanup logf = do
		-- This buffers the log file content in memory.
		-- The log file length is limited to the number of
		-- concurrent jobs, times the number of times a move
		-- (of different files) has been interrupted.
		-- That could grow without bounds given enough time,
		-- so the log is also truncated to the most recent
		-- 100 items.
		modifyLogFile logf gitAnnexMoveLock
			(filter (/= logline) . reverse . take 100 . reverse)

	go logf
		-- Only need to check log when there is a copy.
		| deststartedwithcopy = do
			wasnocopy <- checkLogFile (fromRawFilePath logf) gitAnnexMoveLock
				(== logline)
			if wasnocopy
				then go' False
				else go' deststartedwithcopy
		| otherwise = go' deststartedwithcopy

	go' = a . DestStartedWithCopy
