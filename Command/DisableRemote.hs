{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.DisableRemote where

import Command
import Remote
import Git
import qualified Git.Remote.Remove
import qualified Git.Ref

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "disableremote" SectionSetup
		"stop using a remote"
		paramName
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (remotename:[]) = byName' remotename >>= \case
	Left err -> giveup err
	Right r -> starting "disableremote" ai si $ do
		-- It would be good to remove export databases, fsck
		-- databases, and transfer logs, but all of those are
		-- uuid based, so would need to avoid deleting any if the
		-- same uuid is still in use by another remote.
		
		-- It would be good to remove cred files, but there
		-- is currently no way to list cred files belonging to a
		-- remote.
		
		-- It would be good to remove the AuthToken used for a P2P
		-- remote.

		-- If the remote is private, it would be good to remove
		-- the remote from remote.log and uuid.log in the private
		-- journal, and also to remove any private logs for the
		-- uuid. (Unless there are other remotes using the same
		-- uuid.)

		inRepo $ Git.Remote.Remove.remove remotename
		removeRemoteTrackingBranches remotename
		
		next $ return True
  where
	ai = ActionItemOther (Just (UnquotedString remotename))
	si = SeekInput [remotename]
start _ = giveup "Specify the remote's name."
		
-- Remove any remote branches. 
-- This is done because git remote remove only removes the configured
-- remote tracking branch, not other remote branches.
removeRemoteTrackingBranches :: String -> Annex ()
removeRemoteTrackingBranches remotename = do
	branches <- filter (\b -> branchprefix `isPrefixOf` fromRef b) 
		. map snd
		<$> inRepo Git.Ref.list
	forM_ branches $ \b -> 
		inRepo $ Git.Ref.delete' b
  where
	branchprefix = "refs/remotes/" ++ remotename ++ "/"
