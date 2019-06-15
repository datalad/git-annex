{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Direct where

import Command
import qualified Git
import qualified Git.LsFiles
import qualified Git.Branch
import Config
import Annex.Direct
import Annex.Version

cmd :: Command
cmd = notBareRepo $ noDaemonRunning $
	command "direct" SectionSetup "switch repository to direct mode"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = ifM versionSupportsDirectMode
	( ifM isDirect
		( stop 
		, starting "direct" (ActionItemOther Nothing)
			perform
		)
	, giveup "Direct mode is not supported by this repository version. Use git-annex unlock instead."
	)

perform :: CommandPerform
perform = do
	showOutput
	_ <- inRepo $ Git.Branch.commitCommand Git.Branch.ManualCommit
		[ Param "-a"
		, Param "-m"
		, Param "commit before switching to direct mode"
		]

	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepo [top]
	forM_ l go
	void $ liftIO clean
	next $ return True
  where
	go = whenAnnexed $ \f k -> do
		toDirectGen k f >>= \case
			Nothing -> noop
			Just a -> tryNonAsync a >>= \case
				Left e -> warnlocked f e
				Right _ -> return ()
		return Nothing

	warnlocked :: FilePath -> SomeException -> Annex ()
	warnlocked f e = do
		warning $ f ++ ": " ++ show e
		warning "leaving this file as-is; correct this problem and run git annex fsck on it"
