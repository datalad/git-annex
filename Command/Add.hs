{- git-annex command
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Add where

import Command
import Annex.Ingest
import Logs.Location
import Annex.Content
import qualified Annex
import qualified Annex.Queue
import qualified Database.Keys
import Annex.FileMatcher
import Annex.Link
import Annex.Tmp
import Annex.HashObject
import Annex.WorkTree
import Messages.Progress
import Git.FilePath
import Git.Types
import Git.UpdateIndex
import Config.GitConfig
import Utility.OptParse
import Utility.InodeCache
import Annex.InodeSentinal
import Annex.CheckIgnore
import qualified Utility.RawFilePath as R
import qualified System.FilePath.ByteString as P

import System.PosixCompat.Files (fileSize, isSymbolicLink, isRegularFile, modificationTime, fileID, deviceID, fileMode, ownerExecuteMode, intersectFileModes)

cmd :: Command
cmd = notBareRepo $ 
	withAnnexOptions opts $
		command "add" SectionCommon "add files to annex"
			paramPaths (seek <$$> optParser)
  where
	opts =
		[ backendOption
		, jobsOption
		, jsonOptions
		, jsonProgressOption
		, fileMatchingOptions LimitDiskFiles
		]

data AddOptions = AddOptions
	{ addThese :: CmdParams
	, batchOption :: BatchMode
	, updateOnly :: Bool
	, largeFilesOverride :: Maybe Bool
	, checkGitIgnoreOption :: CheckGitIgnore
	, dryRunOption :: DryRun
	}

optParser :: CmdParamsDesc -> Parser AddOptions
optParser desc = AddOptions
	<$> cmdParams desc
	<*> parseBatchOption False
	<*> switch
		( long "update"
		<> short 'u'
		<> help "only update tracked files"
		)
	<*> (parseforcelarge <|> parseforcesmall)
	<*> checkGitIgnoreSwitch
	<*> parseDryRunOption
  where
	parseforcelarge = flag Nothing (Just True)
		( long "force-large"
		<> help "add all files to annex, ignoring other configuration"
		)
	parseforcesmall = flag Nothing (Just False)
		( long "force-small"
		<> help "add all files to git, ignoring other configuration"
		)

checkGitIgnoreSwitch :: Parser CheckGitIgnore
checkGitIgnoreSwitch = CheckGitIgnore <$>
	invertableSwitch "check-gitignore" True
		(help "Do not check .gitignore when adding files")

seek :: AddOptions -> CommandSeek
seek o = startConcurrency commandStages (seek' o)

seek' :: AddOptions -> CommandSeek
seek' o = do
	largematcher <- largeFilesMatcher
	addunlockedmatcher <- addUnlockedMatcher
	annexdotfiles <- getGitConfigVal annexDotFiles 
	let gofile includingsmall (si, file) = case largeFilesOverride o of
		Nothing -> do
			isdotfile <- if annexdotfiles
				then pure False
				else dotfile . getTopFilePath
					<$> inRepo (toTopFilePath file)
			islarge <- checkFileMatcher NoLiveUpdate largematcher file
				<||> Annex.getRead Annex.force
			if (not isdotfile && islarge)
				then start dr si file addunlockedmatcher
				else if includingsmall
					then ifM (annexAddSmallFiles <$> Annex.getGitConfig)
						( startSmall isdotfile dr si file
						, stop
						)
					else stop
		Just True -> start dr si file addunlockedmatcher
		Just False -> startSmallOverridden dr si file
	case batchOption o of
		Batch fmt
			| updateOnly o ->
				giveup "--update --batch is not supported"
			| otherwise -> batchOnly Nothing (addThese o) $
				batchFiles fmt $ \v@(_si, file) -> 
					ifM (checkIgnored (checkGitIgnoreOption o) file)
						( stop
						, gofile True v
						)
		NoBatch -> do
			-- Avoid git ls-files complaining about files that
			-- are not known to git yet, since this will add
			-- them. Instead, have workTreeItems warn about other
			-- problems, like files that don't exist.
			let ww = WarnUnmatchWorkTreeItems "add"
			l <- workTreeItems ww (addThese o)
			let go b a = a ww (commandAction . gofile b) l
			unless (updateOnly o) $
				go True (withFilesNotInGit (checkGitIgnoreOption o))
			go True withFilesMaybeModified
			-- Convert newly unlocked files back to locked files,
			-- same as a modified unlocked file would get
			-- locked when added.
			go False withUnmodifiedUnlockedPointers
  where
	dr = dryRunOption o

{- Pass file off to git-add. -}
startSmall :: Bool -> DryRun -> SeekInput -> RawFilePath -> CommandStart
startSmall isdotfile dr si file =
	liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
		Just s -> 
			starting "add" (ActionItemTreeFile file) si $
				addSmall isdotfile dr file s
		Nothing -> stop

addSmall :: Bool -> DryRun -> RawFilePath -> FileStatus -> CommandPerform
addSmall isdotfile dr file s = do
	showNote $ (if isdotfile then "dotfile" else "non-large file")
		<> "; adding content to git repository"
	skipWhenDryRun dr $ next $ addFile Small file s

startSmallOverridden :: DryRun -> SeekInput -> RawFilePath -> CommandStart
startSmallOverridden dr si file = 
	liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
		Just s -> starting "add" (ActionItemTreeFile file) si $ do
			showNote "adding content to git repository"
			skipWhenDryRun dr $ next $ addFile Small file s
		Nothing -> stop

data SmallOrLarge = Small | Large

addFile :: SmallOrLarge -> RawFilePath -> FileStatus -> Annex Bool
addFile smallorlarge file s = do
	sha <- if isSymbolicLink s
		then hashBlob =<< liftIO (R.readSymbolicLink file)
		else if isRegularFile s
			then hashFile file
			else do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $
					file <> " is not a regular file"
	let treetype = if isSymbolicLink s
		then TreeSymlink
		else if intersectFileModes ownerExecuteMode (fileMode s) /= 0
			then TreeExecutable
			else TreeFile
	s' <- liftIO $ catchMaybeIO $ R.getSymbolicLinkStatus file
	if maybe True (changed s) s'
		then do
			warning $ QuotedPath file <> " changed while it was being added"
			return False
		else do
			case smallorlarge of
				-- In case the file is being converted from 
				-- an annexed file to be stored in git,
				-- remove the cached inode, so that if the
				-- smudge clean filter later runs on the file,
				-- it will not remember it was annexed.
				Small -> maybe noop Database.Keys.removeInodeCache
					=<< withTSDelta (liftIO . genInodeCache file)
				Large -> noop
			Annex.Queue.addUpdateIndex =<<
				inRepo (stageFile sha treetype file)
			return True
  where
	changed a b =
		deviceID a /= deviceID b ||
		fileID a /= fileID b ||
		fileSize a /= fileSize b ||
		modificationTime a /= modificationTime b ||
		isRegularFile a /= isRegularFile b ||
		isSymbolicLink a /= isSymbolicLink b

start :: DryRun -> SeekInput -> RawFilePath -> AddUnlockedMatcher -> CommandStart
start dr si file addunlockedmatcher = 
	liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
		Nothing -> stop
		Just s
			| not (isRegularFile s) && not (isSymbolicLink s) -> stop
			| otherwise -> do
				mk <- liftIO $ isPointerFile file
				maybe (go s) (fixuppointer s) mk
  where
	go s = lookupKey file >>= \case
		Just k -> addpresent s k
		Nothing -> add s
	add s = starting "add" (ActionItemTreeFile file) si $
		skipWhenDryRun dr $
			if isSymbolicLink s
				then next $ addFile Small file s
				else perform file addunlockedmatcher
	addpresent s key
		| isSymbolicLink s = fixuplink key
		| otherwise = add s
	fixuplink key = 
		starting "add" (ActionItemTreeFile file) si $
			addingExistingLink file key $
				skipWhenDryRun dr $ withOtherTmp $ \tmp -> do
					let tmpf = tmp P.</> P.takeFileName file
					liftIO $ moveFile file tmpf
					ifM (isSymbolicLink <$> liftIO (R.getSymbolicLinkStatus tmpf))
						( do
							liftIO $ R.removeLink tmpf
							addSymlink file key Nothing
							next $ cleanup key =<< inAnnex key
						, do
							liftIO $ moveFile tmpf file
							next $ return True
						)
	fixuppointer s key =
		starting "add" (ActionItemTreeFile file) si $
			addingExistingLink file key $
				skipWhenDryRun dr $ do
					Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
					next $ addFile Large file s

perform :: RawFilePath -> AddUnlockedMatcher -> CommandPerform
perform file addunlockedmatcher = withOtherTmp $ \tmpdir -> do
	lockingfile <- not <$> addUnlocked addunlockedmatcher
		(MatchingFile (FileInfo file file Nothing))
		True
	let cfg = LockDownConfig
		{ lockingFile = lockingfile
		, hardlinkFileTmpDir = Just tmpdir
		, checkWritePerms = True
		}
	ld <- lockDown cfg (fromRawFilePath file)
	let sizer = keySource <$> ld
	v <- metered Nothing sizer Nothing $ \_meter meterupdate ->
		ingestAdd meterupdate ld
	finish v
  where
	finish (Just key) = next $ cleanup key True
	finish Nothing = stop

cleanup :: Key -> Bool -> CommandCleanup
cleanup key hascontent = do
	maybeShowJSON $ JSONChunk [("key", serializeKey key)]
	when hascontent $
		logStatus NoLiveUpdate key InfoPresent
	return True
