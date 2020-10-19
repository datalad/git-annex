{- git-annex command
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
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
import Annex.HashObject
import Messages.Progress
import Git.Types
import Git.FilePath
import Config.GitConfig
import qualified Git.UpdateIndex
import Utility.FileMode
import Utility.OptParse
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
	<*> parseBatchOption
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
	let gofile (si, file) = case largeFilesOverride o of
		Nothing -> 
			let file' = fromRawFilePath file
			in ifM (pure (annexdotfiles || not (dotfile file')) <&&> (checkFileMatcher largematcher file' <||> Annex.getState Annex.force))
				( start o si file addunlockedmatcher
				, ifM (annexAddSmallFiles <$> Annex.getGitConfig)
					( startSmall o si file
					, stop
					)
				)
		Just True -> start o si file addunlockedmatcher
		Just False -> startSmallOverridden o si file
	case batchOption o of
		Batch fmt
			| updateOnly o ->
				giveup "--update --batch is not supported"
			| otherwise -> batchFilesMatching fmt gofile
		NoBatch -> do
			-- Avoid git ls-files complaining about files that
			-- are not known to git yet, since this will add
			-- them. Instead, have workTreeItems warn about other
			-- problems, like files that don't exist.
			let ww = WarnUnmatchWorkTreeItems
			l <- workTreeItems ww (addThese o)
			let go a = a ww (commandAction . gofile) l
			unless (updateOnly o) $
				go (withFilesNotInGit (checkGitIgnoreOption o))
			go withFilesMaybeModified
			go withUnmodifiedUnlockedPointers

{- Pass file off to git-add. -}
startSmall :: AddOptions -> SeekInput -> RawFilePath -> CommandStart
startSmall o si file =
	starting "add" (ActionItemWorkTreeFile file) si $
		next $ addSmall (checkGitIgnoreOption o) file

addSmall :: CheckGitIgnore -> RawFilePath -> Annex Bool
addSmall ci file = do
	showNote "non-large file; adding content to git repository"
	addFile ci file

startSmallOverridden :: AddOptions -> SeekInput -> RawFilePath -> CommandStart
startSmallOverridden o si file = 
	starting "add" (ActionItemWorkTreeFile file) si $
		next $ addSmallOverridden o file

addSmallOverridden :: AddOptions -> RawFilePath -> Annex Bool
addSmallOverridden o file = do
	showNote "adding content to git repository"
	let file' = fromRawFilePath file
	s <- liftIO $ getSymbolicLinkStatus file'
	if not (isRegularFile s)
		then addFile (checkGitIgnoreOption o) file
		else do
			-- Can't use addFile because the clean filter will
			-- honor annex.largefiles and it has been overridden.
			-- Instead, hash the file and add to the index.
			sha <- hashFile file'
			let ty = if isExecutable (fileMode s)
				then TreeExecutable
				else TreeFile
			Annex.Queue.addUpdateIndex =<<
				inRepo (Git.UpdateIndex.stageFile sha ty file')
			return True

addFile :: CheckGitIgnore -> RawFilePath -> Annex Bool
addFile ci file = do
	ps <- gitAddParams ci
	Annex.Queue.addCommand "add" (ps++[Param "--"]) [fromRawFilePath file]
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
				starting "add" (ActionItemWorkTreeFile file) si $
					if isSymbolicLink s
						then next $ addFile (checkGitIgnoreOption o) file
						else perform o file addunlockedmatcher
	addpresent key = 
		liftIO (catchMaybeIO $ R.getSymbolicLinkStatus file) >>= \case
			Just s | isSymbolicLink s -> fixuplink key
			_ -> add
	fixuplink key = starting "add" (ActionItemWorkTreeFile file) si $ do
		-- the annexed symlink is present but not yet added to git
		liftIO $ removeFile (fromRawFilePath file)
		addLink (checkGitIgnoreOption o) (fromRawFilePath file) key Nothing
		next $
			cleanup key =<< inAnnex key
	fixuppointer key = starting "add" (ActionItemWorkTreeFile file) si $ do
		-- the pointer file is present, but not yet added to git
		Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
		next $ addFile (checkGitIgnoreOption o) file

perform :: AddOptions -> RawFilePath -> AddUnlockedMatcher -> CommandPerform
perform o file addunlockedmatcher = withOtherTmp $ \tmpdir -> do
	lockingfile <- not <$> addUnlocked addunlockedmatcher
		(MatchingFile (FileInfo (Just file) file))
	let cfg = LockDownConfig
		{ lockingFile = lockingfile
		, hardlinkFileTmpDir = Just tmpdir
		}
	ld <- lockDown cfg (fromRawFilePath file)
	let sizer = keySource <$> ld
	v <- metered Nothing sizer $ \_meter meterupdate ->
		ingestAdd (checkGitIgnoreOption o) meterupdate ld
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
