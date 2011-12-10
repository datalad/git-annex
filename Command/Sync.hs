{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Sync where

import Common.Annex
import Command
import qualified Annex.Branch
import qualified Git

import qualified Data.ByteString.Lazy.Char8 as L

def :: [Command]
def = [command "sync" paramPaths seek "synchronize local repository with remote"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
        showStart "sync" "."
	showOutput
        next perform

perform :: CommandPerform
perform = do
	remote <- defaultRemote =<< currentBranch
	checkRemote remote
	commit
	inRepo $ Git.run "pull" [Param remote]
	Annex.Branch.update
	inRepo $ Git.run "push" [Param remote, matchingbranches]
	next $ return True
	where
		-- git push may be configured to not push matching
		-- branches; this should ensure it always does.
		matchingbranches = Param ":"

commit :: Annex ()
commit = do
	-- Commit will fail when the tree is clean (or when in a confliced
	-- merge, etc). Ignore failure.
	_ <- inRepo $ Git.runBool "commit" [Param "-a", Param "-m", Param "sync"]
	return ()

-- the remote defaults to origin when not configured
defaultRemote :: String -> Annex String
defaultRemote branch =
	fromRepo $ Git.configGet ("branch." ++ branch ++ ".remote") "origin"

currentBranch :: Annex String
currentBranch = last . split "/" . L.unpack . head . L.lines <$>
	inRepo (Git.pipeRead [Param "symbolic-ref", Param "HEAD"])

checkRemote :: String -> Annex ()
checkRemote remote = do
	remoteurl <- fromRepo $
		Git.configGet ("remote." ++ remote ++ ".url") ""
	when (null remoteurl) $ do
		error $ "No url is configured for the remote: " ++ remote
