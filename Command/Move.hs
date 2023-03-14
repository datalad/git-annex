{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

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
import Logs.Location
import Annex.NumCopies

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $
	command "move" SectionCommon
		"move content of files to/from another repository"
		paramPaths (seek <--< optParser)

data MoveOptions = MoveOptions
	{ moveFiles :: CmdParams
	, fromToOptions :: Maybe FromToHereOptions
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
	<*> parseBatchOption True

instance DeferredParseClass MoveOptions where
	finishParse v = MoveOptions
		<$> pure (moveFiles v)
		<*> maybe (pure Nothing) (Just <$$> finishParse)
			(fromToOptions v)
		<*> pure (removeWhen v)
		<*> pure (keyOptions v)
		<*> pure (batchOption v)

data RemoveWhen = RemoveSafe | RemoveNever
	deriving (Show, Eq)

seek :: MoveOptions -> CommandSeek
seek o = case fromToOptions o of
	Just fto -> seek' o fto
	Nothing -> giveup "Specify --from or --to"

seek' :: MoveOptions -> FromToHereOptions -> CommandSeek
seek' o fto = startConcurrency (stages fto) $ do
	case batchOption o of
		NoBatch -> withKeyOptions (keyOptions o) False seeker
			(commandAction . keyaction)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (moveFiles o)
		Batch fmt -> batchOnly (keyOptions o) (moveFiles o) $
			batchAnnexed fmt seeker keyaction
  where
	seeker = AnnexedFileSeeker
		{ startAction = start fto (removeWhen o)
		, checkContentPresent = case fto of
			FromOrToRemote (FromRemote _) -> Nothing
			FromOrToRemote (ToRemote _) -> Just True
			ToHere -> Nothing
			FromRemoteToRemote _ _ -> Nothing
		, usesLocationLog = True
		}
	keyaction = startKey fto (removeWhen o)
	ww = WarnUnmatchLsFiles

stages :: FromToHereOptions -> UsedStages
stages (FromOrToRemote (FromRemote _)) = transferStages
stages (FromOrToRemote (ToRemote _)) = commandStages
stages ToHere = transferStages
stages (FromRemoteToRemote _ _) = transferStages

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
		FromOrToRemote (FromRemote src) ->
			checkFailedTransferDirection ai Download $
				fromStart removewhen afile key ai si =<< getParsed src
		FromOrToRemote (ToRemote dest) ->
			checkFailedTransferDirection ai Upload $
				toStart removewhen afile key ai si =<< getParsed dest
		ToHere ->
			checkFailedTransferDirection ai Download $
				toHereStart removewhen afile key ai si
		FromRemoteToRemote src dest -> do
			src' <- getParsed src
			dest' <- getParsed dest
			fromToStart removewhen afile key ai si src' dest'

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
	fast <- Annex.getRead Annex.fast
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
toPerform = toPerform' Nothing

toPerform' :: Maybe ContentRemovalLock -> Remote -> RemoveWhen -> Key -> AssociatedFile -> Bool -> Either String Bool -> CommandPerform
toPerform' mcontentlock dest removewhen key afile fastcheck isthere = do
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
					logMoveCleanup deststartedwithcopy
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
			logMoveCleanup deststartedwithcopy
			next $ return True
		RemoveSafe -> lockcontentforremoval $ \contentlock -> do
			srcuuid <- getUUID
			r <- willDropMakeItWorse srcuuid destuuid deststartedwithcopy key afile >>= \case
				DropAllowed -> drophere setpresentremote contentlock "moved"
				DropCheckNumCopies -> do
					(numcopies, mincopies) <- getSafestNumMinCopies afile key
					(tocheck, verified) <- verifiableCopies key [srcuuid]
					verifyEnoughCopiesToDrop "" key (Just contentlock)
						 numcopies mincopies [srcuuid] verified
						 (UnVerifiedRemote dest : tocheck)
						 (drophere setpresentremote contentlock . showproof)
						 (faileddrophere setpresentremote)
				DropWorse -> faileddrophere setpresentremote
			logMoveCleanup deststartedwithcopy
			return r
	showproof proof = "proof: " ++ show proof
	drophere setpresentremote contentlock reason = do
		fastDebug "Command.Move" $ unwords
			[ "Dropping from here"
			, "(" ++ reason ++ ")"
			]
		-- Drop content before updating location logs,
		-- in case disk space is very low this frees
		-- up space before writing data to disk.
		removeAnnex contentlock
		next $ do
			() <- setpresentremote
			Command.Drop.cleanupLocal key (Command.Drop.DroppingUnused False)
	faileddrophere setpresentremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote "Content not dropped from here."
		next $ do
			() <- setpresentremote
			return False
	
	lockcontentforremoval a = case mcontentlock of
		Nothing -> lockContentForRemoval key lockfailed a
		Just contentlock -> a contentlock
	
	-- This occurs when, for example, two files are being dropped
	-- and have the same content. The seek stage checks if the content
	-- is present, but due to buffering, may find it present for the
	-- second file before the first is dropped. If so, nothing remains
	-- to be done except for cleaning up.
	lockfailed = next $ Command.Drop.cleanupLocal key (Command.Drop.DroppingUnused False)

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
	present <- inAnnex key
	finish <- fromPerform' present True src key afile
	finish removewhen

fromPerform' :: Bool -> Bool -> Remote -> Key -> AssociatedFile -> Annex (RemoveWhen -> CommandPerform)
fromPerform' present updatelocationlog src key afile = do
	showAction $ "from " ++ Remote.name src
	destuuid <- getUUID
	logMove (Remote.uuid src) destuuid present key $ \deststartedwithcopy ->
		if present
			then return $ finish deststartedwithcopy True
			else do
				got <- get
				return $ finish deststartedwithcopy got
  where
	get = notifyTransfer Download afile $
		logdownload .
			download src key afile stdRetry
	
	logdownload a
		| updatelocationlog = logStatusAfter key a
		| otherwise = a

	finish deststartedwithcopy False _ = do
		logMoveCleanup deststartedwithcopy
		stop -- copy failed
	finish deststartedwithcopy True RemoveNever = do
		logMoveCleanup deststartedwithcopy
		next $ return True -- copy complete
	finish deststartedwithcopy True RemoveSafe = do
		destuuid <- getUUID
		lockContentShared key $ \_lck ->
			fromDrop src destuuid deststartedwithcopy key afile id

fromDrop :: Remote -> UUID -> DestStartedWithCopy -> Key -> AssociatedFile -> ([UnVerifiedCopy] -> [UnVerifiedCopy])-> CommandPerform
fromDrop src destuuid deststartedwithcopy key afile adjusttocheck =
	willDropMakeItWorse (Remote.uuid src) destuuid deststartedwithcopy key afile >>= \case
		DropAllowed -> dropremote "moved"
		DropCheckNumCopies -> do
			(numcopies, mincopies) <- getSafestNumMinCopies afile key
			(tocheck, verified) <- verifiableCopies key [Remote.uuid src]
			verifyEnoughCopiesToDrop "" key Nothing numcopies mincopies [Remote.uuid src] verified
				(adjusttocheck tocheck) (dropremote . showproof) faileddropremote
		DropWorse -> faileddropremote
  where
	showproof proof = "proof: " ++ show proof

	dropremote reason = do
		fastDebug "Command.Move" $ unwords
			[ "Dropping from remote"
			, show src
			, "(" ++ reason ++ ")"
			]
		ok <- Remote.action (Remote.removeKey src key)
		when ok $
			logMoveCleanup deststartedwithcopy
		next $ Command.Drop.cleanupRemote key src (Command.Drop.DroppingUnused False) ok

	faileddropremote = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		showLongNote $ "Content not dropped from " ++ Remote.name src ++ "."
		logMoveCleanup deststartedwithcopy
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

fromToStart :: RemoveWhen -> AssociatedFile -> Key -> ActionItem -> SeekInput -> Remote -> Remote -> CommandStart
fromToStart removewhen afile key ai si src dest = do
	if Remote.uuid src == Remote.uuid dest
		then stop
		else do
			u <- getUUID
			if u == Remote.uuid src
				then toStart removewhen afile key ai si dest
				else if u == Remote.uuid dest
					then fromStart removewhen afile key ai si src
					else stopUnless (fromOk src key) $
						starting (describeMoveAction removewhen) (OnlyActionOn key ai) si $
							fromToPerform src dest removewhen key afile

{- When there is a local copy, transfer it to the dest, and drop from the src.
 -
 - When the dest has a copy, drop it from the src.
 -
 - Otherwise, download a copy from the dest, populating the local annex
 - copy, but not updating location logs. Then transfer that to the dest,
 - drop the local copy, and finally drop from the src.
 -
 - Using a regular download of the local copy, rather than download to
 - some other file makes resuming an interrupted download work as usual,
 - and simplifies implementation. It does mean that, if `git-annex get` of
 - the same content is being run at the same time as this move, the content
 - may end up locally present, or not. This is similar to the behavior 
 - when running `git-annex move --to` concurrently with git-annex get.
 -}
fromToPerform :: Remote -> Remote -> RemoveWhen -> Key -> AssociatedFile -> CommandPerform
fromToPerform src dest removewhen key afile = do
	hereuuid <- getUUID
	loggedpresent <- any (== hereuuid)
		<$> loggedLocations key
	ispresent <- inAnnex key
	go ispresent loggedpresent
  where
	-- The content is present, and is logged as present, so it
	-- can be sent to dest and dropped from src.
	--
	-- When resuming an interrupted move --from --to, where the content
	-- was not present but got downloaded from src, it will not be
	-- logged present, and so this won't be used. Instead, the local
	-- content will get dropped after being copied to dest.
	go True True = do
		haskey <- Remote.hasKey dest key
		-- Prepare to drop from src later. Doing this first
		-- makes "from src" be shown consistently before
		-- "to dest"
		dropsrc <- fromsrc True
		combinecleanups 
			-- Send to dest, preserve local copy.
			(todest Nothing RemoveNever haskey)
			(\senttodest -> if senttodest
				then dropsrc removewhen
				else stop
			)
	go ispresent _loggedpresent = do
		haskey <- Remote.hasKey dest key
		case haskey of
                	Left err -> do                   
				showNote err       
				stop
			Right True -> do
				showAction $ "from " ++ Remote.name src
				showAction $ "to " ++ Remote.name dest
				-- The log may not indicate dest's copy
				-- yet, so make sure it does.
				logChange key (Remote.uuid dest) InfoPresent
				-- Drop from src, checking copies including
				-- the one already in dest.
				dropfromsrc id
			Right False -> do
				-- Get local copy from src, defer dropping
				-- from src until later. Note that fromsrc
				-- does not update the location log.
				cleanupfromsrc <- if ispresent
					then return $ const $ next (return True)
					else fromsrc False
				-- Lock the local copy for removal early,
				-- to avoid other processes relying on it
				-- as a copy, and removing other copies
				-- (such as the one in src), that prevents
				-- dropping the local copy later.
				lockContentForRemoval key stop $ \contentlock ->
					combinecleanups
						-- Send to dest and remove local copy.
						(todest (Just contentlock) RemoveSafe haskey)
						(\senttodest ->
							-- Drop from src, checking
							-- copies including dest.
							combinecleanups
								(cleanupfromsrc RemoveNever)
								(\_ -> if senttodest
									then dropfromsrc (\l -> UnVerifiedRemote dest : l)
									else stop
								)
						)

	fromsrc present = fromPerform' present False src key afile

	todest mcontentlock removewhen' = toPerform' mcontentlock dest removewhen' key afile False

	dropfromsrc adjusttocheck = case removewhen of
		RemoveSafe -> logMove (Remote.uuid src) (Remote.uuid dest) True key $ \deststartedwithcopy ->
			fromDrop src (Remote.uuid dest) deststartedwithcopy key afile adjusttocheck
		RemoveNever -> next (return True)

	combinecleanups a b = a >>= \case
		Just cleanupa -> b True >>= \case
			Just cleanupb -> return $ Just $ do
				oka <- cleanupa
				okb <- cleanupb
				return (oka && okb)
			Nothing -> return (Just cleanupa)
		Nothing -> b False >>= \case
			Just cleanupb -> return $ Just $ do
				void cleanupb
				return False
			Nothing -> return Nothing

{- The goal of this command is to allow the user maximum freedom to move
 - files as they like, while avoiding making bad situations any worse
 - than they already were.
 -
 - When the destination repository already had a copy of a file
 - before the move operation began, dropping it from the source
 - repository reduces the number of copies, and should fail if
 - that would violate numcopies settings.
 -
 - On the other hand, when the destination repository did not start
 - with a copy of a file, it can be dropped from the source without
 - making numcopies worse, so the move is allowed even if numcopies
 - is not met.
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
willDropMakeItWorse srcuuid destuuid (DestStartedWithCopy deststartedwithcopy _) key afile =
	ifM (Command.Drop.checkRequiredContent (Command.Drop.PreferredContentChecked False) srcuuid key afile)
		( if deststartedwithcopy
			then unlessforced DropCheckNumCopies
			else ifM checktrustlevel
				( return DropAllowed
				, unlessforced DropCheckNumCopies
				)
		, unlessforced DropWorse
		)
  where
	unlessforced r = ifM (Annex.getRead Annex.force)
		( return DropAllowed
		, return r
		)
	checktrustlevel = do
		desttrust <- lookupTrust destuuid
		srctrust <- lookupTrust srcuuid
		return (desttrust > UnTrusted || desttrust >= srctrust)

data DropCheck = DropWorse | DropAllowed | DropCheckNumCopies

data DestStartedWithCopy = DestStartedWithCopy Bool (Annex ())

{- This should be called once the move has succeeded, or if it failed
 - without doing anything. It should not be called if the move transferred
 - the content but failed to drop due to eg a network error. In such a
 - case, the move can be restarted later, so the move log should be
 - preserved. -}
logMoveCleanup :: DestStartedWithCopy -> Annex ()
logMoveCleanup (DestStartedWithCopy _ a) = a

{- Runs an action that performs a move, and logs the move, allowing an
 - failed or interrupted move to be re-done later.
 -
 - This deals with the situation where dest did not start with a copy,
 - but the move downloaded it, and was then interrupted before dropping
 - it from the source. Re-running the move would see dest has a
 - copy, and so could refuse to allow the drop. By providing the logged
 - DestStartedWithCopy, this avoids that annoyance.
 -}
logMove :: UUID -> UUID -> Bool -> Key -> (DestStartedWithCopy -> Annex a) -> Annex a
logMove srcuuid destuuid deststartedwithcopy key a = go =<< setup
  where
	logline = L.fromStrict $ B8.unwords
		[ fromUUID srcuuid
		, fromUUID destuuid
		, serializeKey' key
		]

	setup = do
		logf <- fromRepo gitAnnexMoveLog
		lckf <- fromRepo gitAnnexMoveLock
		-- Only log when there was no copy.
		unless deststartedwithcopy $
			appendLogFile logf lckf logline
		return (logf, lckf)

	cleanup (logf, lckf) =
		-- This buffers the log file content in memory.
		-- The log file length is limited to the number of
		-- concurrent jobs, times the number of times a move
		-- (of different files) has been interrupted.
		-- That could grow without bounds given enough time,
		-- so the log is also truncated to the most recent
		-- 100 items.
		modifyLogFile logf lckf
			(filter (/= logline) . reverse . take 100 . reverse)

	go fs@(logf, lckf)
		-- Only need to check log when there is a copy.
		| deststartedwithcopy = do
			wasnocopy <- checkLogFile logf lckf (== logline)
			if wasnocopy
				then go' fs False
				else go' fs deststartedwithcopy
		| otherwise = go' fs deststartedwithcopy

	go' fs deststartedwithcopy' = a $
		DestStartedWithCopy deststartedwithcopy' (cleanup fs)
