{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Direct where

import Control.Exception.Extensible

import Common.Annex
import Command
import qualified Git
import qualified Git.Command
import qualified Git.LsFiles
import Config
import Annex.Direct
import Annex.Exception

def :: [Command]
def = [notBareRepo $ noDaemonRunning $
	command "direct" paramNothing seek
		SectionSetup "switch repository to direct mode"]

seek :: CommandSeek
seek = withNothing start

start :: CommandStart
start = ifM isDirect ( stop , next perform )

perform :: CommandPerform
perform = do
	showStart "commit" ""
	showOutput
	_ <- inRepo $ Git.Command.runBool
		[ Param "commit"
		, Param "-a"
		, Param "-m"
		, Param "commit before switching to direct mode"
		]
	showEndOk

	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepo [top]
	forM_ l go
	void $ liftIO clean
	next cleanup
  where
	go = whenAnnexed $ \f (k, _) -> do
		r <- toDirectGen k f
		case r of
			Nothing -> noop
			Just a -> do
				showStart "direct" f
				r' <- tryAnnex a
				case r' of
					Left e -> warnlocked e
					Right _ -> showEndOk
		return Nothing

	warnlocked :: SomeException -> Annex ()
	warnlocked e = do
		warning $ show e
		warning "leaving this file as-is; correct this problem and run git annex fsck on it"

cleanup :: CommandCleanup
cleanup = do
	showStart "direct" ""
	setDirect True
	return True
