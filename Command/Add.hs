{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Add where

import Common.Annex
import Annex.Exception
import Command
import qualified Annex
import qualified Annex.Queue
import Backend
import Logs.Location
import Annex.Content
import Annex.Perms
import Utility.Touch
import Utility.FileMode

def :: [Command]
def = [command "add" paramPaths seek "add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files. -}
seek :: [CommandSeek]
seek = [withFilesNotInGit start, withFilesUnlocked start]

{- The add subcommand annexes a file, storing it in a backend, and then
 - moving it into the annex directory and setting up the symlink pointing
 - to its content. -}
start :: FilePath -> CommandStart
start file = notBareRepo $ ifAnnexed file fixup add
	where
		add = do
			s <- liftIO $ getSymbolicLinkStatus file
			if isSymbolicLink s || not (isRegularFile s)
				then stop
				else do
					showStart "add" file
					next $ perform file
		fixup (key, _) = do
			-- fixup from an interrupted add; the symlink
			-- is present but not yet added to git
			showStart "add" file
			liftIO $ removeFile file
			next $ next $ cleanup file key =<< inAnnex key

{- The file that's being added is locked down before a key is generated,
 - to prevent it from being modified in between. It's hard linked into a
 - temporary location, and its writable bits are removed. It could still be
 - written to by a process that already has it open for writing. -}
perform :: FilePath -> CommandPerform
perform file = do
	liftIO $ preventWrite file
	tmp <- fromRepo gitAnnexTmpDir
	createAnnexDirectory tmp
	pid <- liftIO getProcessID
	let tmpfile = tmp </> "add" ++ show pid ++ "." ++ takeFileName file
	nuke tmpfile
	liftIO $ createLink file tmpfile
	let source = KeySource { keyFilename = file, contentLocation = tmpfile }
	backend <- chooseBackend file
	genKey source backend >>= go tmpfile
	where
		go _ Nothing = stop
		go tmpfile (Just (key, _)) = do
			handle (undo file key) $ moveAnnex key tmpfile
			nuke file
			next $ cleanup file key True

nuke :: FilePath -> Annex ()
nuke file = liftIO $ whenM (doesFileExist file) $ removeFile file

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
undo :: FilePath -> Key -> IOException -> Annex a
undo file key e = do
	whenM (inAnnex key) $ do
		nuke file
		handle tryharder $ fromAnnex key file
		logStatus key InfoMissing
	throw e
	where
		-- fromAnnex could fail if the file ownership is weird
		tryharder :: IOException -> Annex ()
		tryharder _ = do
			src <- inRepo $ gitAnnexLocation key
			liftIO $ moveFile src file

cleanup :: FilePath -> Key -> Bool -> CommandCleanup
cleanup file key hascontent = do
	handle (undo file key) $ do
		link <- calcGitLink file key
		liftIO $ createSymbolicLink link file

		when hascontent $ do
			logStatus key InfoPresent
	
			-- touch the symlink to have the same mtime as the
			-- file it points to
			liftIO $ do
				mtime <- modificationTime <$> getFileStatus file
				touch file (TimeSpec mtime) False

	params <- ifM (Annex.getState Annex.force)
		( return [Param "-f"]
		, return []
		)
	Annex.Queue.add "add" (params++[Param "--"]) [file]
	return True
