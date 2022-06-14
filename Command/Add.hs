{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

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
import Messages.Progress
import Git.FilePath
import Config.GitConfig
import Config.Smudge
import Utility.OptParse
import Utility.InodeCache
import Annex.InodeSentinal
import Annex.CheckIgnore
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = notBareRepo $ 
	withGlobalOptions opts $
		command "add" SectionCommon "add files to annex"
			paramPaths (seek <$$> optParser)
  where
	opts =
		[ jobsOption
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
seek o = startConcurrency commandStages $ do
	largematcher <- largeFilesMatcher
	addunlockedmatcher <- addUnlockedMatcher
	annexdotfiles <- getGitConfigVal annexDotFiles 
	let gofile includingsmall (si, file) = case largeFilesOverride o of
		Nothing -> 
			ifM (pure (annexdotfiles || not (dotfile file))
				<&&> (checkFileMatcher largematcher file 
				<||> Annex.getState Annex.force))
				( start o si file addunlockedmatcher
				, if includingsmall
					then ifM (annexAddSmallFiles <$> Annex.getGitConfig)
						( startSmall o si file
						, stop
						)
					else stop
				)
		Just True -> start o si file addunlockedmatcher
		Just False -> startSmallOverridden o si file
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
			let ww = WarnUnmatchWorkTreeItems
			l <- workTreeItems ww (addThese o)
			let go b a = a ww (commandAction . gofile b) l
			unless (updateOnly o) $
				go True (withFilesNotInGit (checkGitIgnoreOption o))
			go True withFilesMaybeModified
			-- Convert newly unlocked files back to locked files,
			-- same as a modified unlocked file would get
			-- locked when added.
			go False withUnmodifiedUnlockedPointers

{- Pass file off to git-add. -}
startSmall :: AddOptions -> SeekInput -> RawFilePath -> CommandStart
startSmall o si file =
	starting "add" (ActionItemTreeFile file) si $
		next $ addSmall (checkGitIgnoreOption o) file

addSmall :: CheckGitIgnore -> RawFilePath -> Annex Bool
addSmall ci file = do
	showNote "non-large file; adding content to git repository"
	addFile Small ci file

startSmallOverridden :: AddOptions -> SeekInput -> RawFilePath -> CommandStart
startSmallOverridden o si file = 
	starting "add" (ActionItemTreeFile file) si $ next $ do
		showNote "adding content to git repository"
		addFile Small (checkGitIgnoreOption o) file

data SmallOrLarge = Small | Large

addFile :: SmallOrLarge -> CheckGitIgnore -> RawFilePath -> Annex Bool
addFile smallorlarge ci file = do
	ps <- gitAddParams ci
	cps <- case smallorlarge of
		-- In case the file is being converted from an annexed file
		-- to be stored in git, remove the cached inode, so that
		-- if the smudge clean filter later runs on the file,
		-- it will not remember it was annexed.
		--
		-- The use of bypassSmudgeConfig prevents the smudge
		-- filter from being run. So the changes to the database
		-- can be queued up and not flushed to disk immediately.
		Small -> do
			maybe noop Database.Keys.removeInodeCache
				=<< withTSDelta (liftIO . genInodeCache file)
			return bypassSmudgeConfig
		Large -> return []
	Annex.Queue.addCommand cps "add" (ps++[Param "--"])
		[fromRawFilePath file]
	return True

start :: AddOptions -> SeekInput -> RawFilePath -> AddUnlockedMatcher -> CommandStart
start o si file addunlockedmatcher = do
	mk <- liftIO $ isPointerFile file
	maybe go fixuppointer mk
  where
	go = ifAnnexed file addpresent add
	add = liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
		Nothing -> stop
		Just s 
			| not (isRegularFile s) && not (isSymbolicLink s) -> stop
			| otherwise -> 
				starting "add" (ActionItemTreeFile file) si $
					if isSymbolicLink s
						then next $ addFile Small (checkGitIgnoreOption o) file
						else perform file addunlockedmatcher
	addpresent key = 
		liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
			Just s | isSymbolicLink s -> fixuplink key
			_ -> add
	fixuplink key = 
		starting "add" (ActionItemTreeFile file) si $
			addingExistingLink file key $
				withOtherTmp $ \tmp -> do
					let tmpf = fromRawFilePath tmp </> fromRawFilePath file
					liftIO $ moveFile (fromRawFilePath file) tmpf
					ifM (isSymbolicLink <$> liftIO (getSymbolicLinkStatus tmpf))
						( do
							liftIO $ removeFile tmpf
							addSymlink file key Nothing
							next $ cleanup key =<< inAnnex key
						, do
							liftIO $ moveFile tmpf (fromRawFilePath file)
							next $ return True
						)
	fixuppointer key =
		starting "add" (ActionItemTreeFile file) si $
			addingExistingLink file key $ do
				Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
				next $ addFile Large (checkGitIgnoreOption o) file

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
		logStatus key InfoPresent
	return True
