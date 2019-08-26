{- git-annex command
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
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
import Config
import Annex.FileMatcher
import Annex.Link
import Annex.Version
import Annex.Tmp
import Messages.Progress
import Git.FilePath

cmd :: Command
cmd = notBareRepo $ 
	withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption, fileMatchingOptions] $
		command "add" SectionCommon "add files to annex"
			paramPaths (seek <$$> optParser)

data AddOptions = AddOptions
	{ addThese :: CmdParams
	, includeDotFiles :: Bool
	, batchOption :: BatchMode
	, updateOnly :: Bool
	}

optParser :: CmdParamsDesc -> Parser AddOptions
optParser desc = AddOptions
	<$> cmdParams desc
	<*> switch
		( long "include-dotfiles"
		<> help "don't skip dotfiles"
		)
	<*> parseBatchOption
	<*> switch
		( long "update"
		<> short 'u'
		<> help "only update tracked files"
		)

seek :: AddOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	matcher <- largeFilesMatcher
	let gofile file = ifM (checkFileMatcher matcher file <||> Annex.getState Annex.force)
		( start file
		, ifM (annexAddSmallFiles <$> Annex.getGitConfig)
			( startSmall file
			, stop
			)
		)
	case batchOption o of
		Batch fmt
			| updateOnly o ->
				giveup "--update --batch is not supported"
			| otherwise -> batchFilesMatching fmt gofile
		NoBatch -> do
			l <- workTreeItems (addThese o)
			let go a = a (commandAction . gofile) l
			unless (updateOnly o) $
				go (withFilesNotInGit (not $ includeDotFiles o))
			go withFilesMaybeModified
			ifM versionSupportsUnlockedPointers
				( go withUnmodifiedUnlockedPointers
				, go withFilesOldUnlocked
				)

{- Pass file off to git-add. -}
startSmall :: FilePath -> CommandStart
startSmall file = starting "add" (ActionItemWorkTreeFile file) $
	next $ addSmall file

addSmall :: FilePath -> Annex Bool
addSmall file = do
	showNote "non-large file; adding content to git repository"
	addFile file

addFile :: FilePath -> Annex Bool
addFile file = do
	ps <- forceParams
	Annex.Queue.addCommand "add" (ps++[Param "--"]) [file]
	return True

start :: FilePath -> CommandStart
start file = do
	ifM versionSupportsUnlockedPointers
		( do
			mk <- liftIO $ isPointerFile file
			maybe go fixuppointer mk
		, go
		)
  where
	go = ifAnnexed file addpresent add
	add = liftIO (catchMaybeIO $ getSymbolicLinkStatus file) >>= \case
		Nothing -> stop
		Just s 
			| not (isRegularFile s) && not (isSymbolicLink s) -> stop
			| otherwise -> 
				starting "add" (ActionItemWorkTreeFile file) $
					if isSymbolicLink s
						then next $ addFile file
						else perform file
	addpresent key = ifM versionSupportsUnlockedPointers
		( liftIO (catchMaybeIO $ getSymbolicLinkStatus file) >>= \case
			Just s | isSymbolicLink s -> fixuplink key
			_ -> add
		, fixuplink key
		)
	fixuplink key = starting "add" (ActionItemWorkTreeFile file) $ do
		-- the annexed symlink is present but not yet added to git
		liftIO $ removeFile file
		addLink file key Nothing
		next $
			cleanup key =<< inAnnex key
	fixuppointer key = starting "add" (ActionItemWorkTreeFile file) $ do
		-- the pointer file is present, but not yet added to git
		Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
		next $ addFile file

perform :: FilePath -> CommandPerform
perform file = withOtherTmp $ \tmpdir -> do
	lockingfile <- not <$> addUnlocked
	let cfg = LockDownConfig
		{ lockingFile = lockingfile
		, hardlinkFileTmpDir = Just tmpdir
		}
	ld <- lockDown cfg file
	let sizer = keySource <$> ld
	v <- metered Nothing sizer $ \_meter meterupdate ->
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
