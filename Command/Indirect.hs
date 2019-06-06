{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Indirect where

import Command
import qualified Git
import qualified Git.Branch
import qualified Git.LsFiles
import Git.FileMode
import Config
import qualified Annex
import Annex.Direct
import Annex.Content
import Annex.Content.Direct
import Annex.CatFile
import Annex.Init
import Annex.Ingest

cmd :: Command
cmd = notBareRepo $ noDaemonRunning $
	command "indirect" SectionSetup "switch repository to indirect mode"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = ifM isDirect
	( do
		unlessM (coreSymlinks <$> Annex.getGitConfig) $
			giveup "Git is configured to not use symlinks, so you must use direct mode."
		whenM probeCrippledFileSystem $
			giveup "This repository seems to be on a crippled filesystem, you must use direct mode."
		starting "indirect" (ActionItemOther Nothing) 
			perform
	, stop
	)

perform :: CommandPerform
perform = do
	whenM stageDirect $ do
		showOutput
		void $ inRepo $ Git.Branch.commitCommand Git.Branch.ManualCommit
			[ Param "-m"
			, Param "commit before switching to indirect mode"
			]

	-- Note that we set indirect mode early, so that we can use
	-- moveAnnex in indirect mode.
	setDirect False

	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.stagedOthersDetails [top]
	forM_ l go
	void $ liftIO clean
	next $ return True
  where
	{- Walk tree from top and move all present direct mode files into
	 - the annex, replacing with symlinks. Also delete direct mode
	 - caches and mappings. -}
	go (f, Just sha, Just mode) | isSymLink mode = do
		r <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus f
		case r of
			Just s
				| isSymbolicLink s -> void $ flip whenAnnexed f $
					\_ k -> do
						removeInodeCache k
						removeAssociatedFiles k
						return Nothing
				| otherwise -> 
					maybe noop (fromdirect f)
						=<< catKey sha
			_ -> noop
	go _ = noop

	fromdirect f k = do
		removeInodeCache k
		removeAssociatedFiles k
		whenM (liftIO $ not . isSymbolicLink <$> getSymbolicLinkStatus f) $ do
			v <- tryNonAsync (moveAnnex k f)
			case v of
				Right True -> do 
					l <- calcRepo $ gitAnnexLink f k
					liftIO $ createSymbolicLink l f
				Right False -> warnlocked "Failed to move file to annex"
				Left e -> catchNonAsync (restoreFile f k e) $
					warnlocked . show

	warnlocked msg = do
		warning msg
		warning "leaving this file as-is; correct this problem and run git annex add on it"
