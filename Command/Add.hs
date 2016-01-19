{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Common.Annex
import Command
import Annex.Ingest
import Logs.Location
import Annex.Content
import Annex.Content.Direct
import Annex.Link
import qualified Annex
import qualified Annex.Queue
import Config
import Utility.InodeCache
import Annex.FileMatcher
import Annex.Version
import qualified Database.Keys

cmd :: Command
cmd = notBareRepo $ withGlobalOptions (jobsOption : fileMatchingOptions) $
	command "add" SectionCommon "add files to annex"
		paramPaths (seek <$$> optParser)

data AddOptions = AddOptions
	{ addThese :: CmdParams
	, includeDotFiles :: Bool
	}

optParser :: CmdParamsDesc -> Parser AddOptions
optParser desc = AddOptions
	<$> cmdParams desc
	<*> switch
		( long "include-dotfiles"
		<> help "don't skip dotfiles"
		)

{- Add acts on both files not checked into git yet, and unlocked files.
 -
 - In direct mode, it acts on any files that have changed. -}
seek :: AddOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	matcher <- largeFilesMatcher
	let go a = flip a (addThese o) $ \file -> ifM (checkFileMatcher matcher file <||> Annex.getState Annex.force)
		( start file
		, startSmall file
		)
	go $ withFilesNotInGit (not $ includeDotFiles o)
	ifM (versionSupportsUnlockedPointers <||> isDirect)
		( go withFilesMaybeModified
		, go withFilesOldUnlocked
		)

{- Pass file off to git-add. -}
startSmall :: FilePath -> CommandStart
startSmall file = do
	showStart "add" file
	next $ next $ addSmall file

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
start file = ifAnnexed file addpresent add
  where
	add = do
		ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Nothing -> stop
			Just s 
				| not (isRegularFile s) && not (isSymbolicLink s) -> stop
				| otherwise -> do
					showStart "add" file
					next $ if isSymbolicLink s
						then next $ addFile file
						else perform file
	addpresent key = ifM versionSupportsUnlockedPointers
		( do
			ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
			case ms of
				Just s | isSymbolicLink s -> fixup key
				_ -> ifM (sameInodeCache file =<< Database.Keys.getInodeCaches key)
						( stop, add )
		, ifM isDirect
			( do
				ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
				case ms of
					Just s | isSymbolicLink s -> fixup key
					_ -> ifM (goodContent key file)
						( stop , add )
			, fixup key
			)
		)
	fixup key = do
		-- the annexed symlink is present but not yet added to git
		showStart "add" file
		liftIO $ removeFile file
		whenM isDirect $
			void $ addAssociatedFile key file
		next $ next $ cleanup file key Nothing =<< inAnnex key

perform :: FilePath -> CommandPerform
perform file = do
	lockingfile <- not <$> isDirect
	let cfg = LockDownConfig
		{ lockingFile = lockingfile
		, hardlinkFileTmp = True
		}
	lockDown cfg file >>= ingest >>= go
  where
	go (Just key, cache) = next $ cleanup file key cache True
	go (Nothing, _) = stop

cleanup :: FilePath -> Key -> Maybe InodeCache -> Bool -> CommandCleanup
cleanup file key mcache hascontent = do
	ifM (isDirect <&&> pure hascontent)
		( do
			l <- calcRepo $ gitAnnexLink file key
			stageSymlink file =<< hashSymlink l
		, addLink file key mcache
		)
	when hascontent $
		logStatus key InfoPresent
	return True
