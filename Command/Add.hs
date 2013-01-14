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
import Types.KeySource
import Backend
import Logs.Location
import Annex.Content
import Annex.Content.Direct
import Annex.Perms
import Utility.Touch
import Utility.FileMode
import Config
import qualified Git.HashObject
import qualified Git.UpdateIndex
import Git.Types

def :: [Command]
def = [notBareRepo $ command "add" paramPaths seek "add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files. -}
seek :: [CommandSeek]
seek =
	[ withFilesNotInGit start
	, whenNotDirect $ withFilesUnlocked start
	]

{- The add subcommand annexes a file, generating a key for it using a
 - backend, and then moving it into the annex directory and setting up
 - the symlink pointing to its content. -}
start :: FilePath -> CommandStart
start file = ifAnnexed file fixup add
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
 - written to by a process that already has it open for writing.
 -
 - Lockdown can fail if a file gets deleted, and Nothing will be returned.
 -}
lockDown :: FilePath -> Annex (Maybe KeySource)
lockDown file = do
	tmp <- fromRepo gitAnnexTmpDir
	createAnnexDirectory tmp
	liftIO $ catchMaybeIO $ do
		preventWrite file
		(tmpfile, h) <- openTempFile tmp (takeFileName file)
		hClose h
		nukeFile tmpfile
		createLink file tmpfile
		return $ KeySource { keyFilename = file , contentLocation = tmpfile }

{- Moves a locked down file into the annex.
 -
 - In direct mode, leaves the file alone, and just updates bookkeeping
 - information.
 -}
ingest :: (Maybe KeySource) -> Annex (Maybe Key)
ingest Nothing = return Nothing
ingest (Just source) = do
	backend <- chooseBackend $ keyFilename source
	ifM isDirect
		( do
			mstat <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus $ keyFilename source
			k <- genKey source backend
			godirect k (toCache =<< mstat)
		, go =<< genKey source backend
		)
  where
	go (Just (key, _)) = do
		handle (undo (keyFilename source) key) $
			moveAnnex key $ contentLocation source
		liftIO $ nukeFile $ keyFilename source
		return $ Just key
	go Nothing = failure

	godirect (Just (key, _)) (Just cache) =
		ifM (compareCache (keyFilename source) $ Just cache)
			( do
				writeCache key cache
				void $ addAssociatedFile key $ keyFilename source
				liftIO $ allowWrite $ keyFilename source
				liftIO $ nukeFile $ contentLocation source
				return $ Just key
			, failure
			)
	godirect _ _ = failure

	failure = do
		liftIO $ nukeFile $ contentLocation source
		return Nothing		

perform :: FilePath -> CommandPerform
perform file = 
	maybe stop (\key -> next $ cleanup file key True)
		=<< ingest =<< lockDown file

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
undo :: FilePath -> Key -> IOException -> Annex a
undo file key e = do
	whenM (inAnnex key) $ do
		liftIO $ nukeFile file
		handle tryharder $ fromAnnex key file
		logStatus key InfoMissing
	throw e
  where
	-- fromAnnex could fail if the file ownership is weird
	tryharder :: IOException -> Annex ()
	tryharder _ = do
		src <- inRepo $ gitAnnexLocation key
		liftIO $ moveFile src file

{- Creates the symlink to the annexed content, returns the link target. -}
link :: FilePath -> Key -> Bool -> Annex String
link file key hascontent = handle (undo file key) $ do
	l <- calcGitLink file key
	liftIO $ createSymbolicLink l file

	when hascontent $ do
		logStatus key InfoPresent
	
		-- touch the symlink to have the same mtime as the
		-- file it points to
		liftIO $ do
			mtime <- modificationTime <$> getFileStatus file
			touch file (TimeSpec mtime) False

	return l

{- Note: Several other commands call this, and expect it to 
 - create the symlink and add it. -}
cleanup :: FilePath -> Key -> Bool -> CommandCleanup
cleanup file key hascontent = ifM (isDirect <&&> pure hascontent)
	( do
		l <- calcGitLink file key
		sha <- inRepo $ Git.HashObject.hashObject BlobObject l
		Annex.Queue.addUpdateIndex =<<
			inRepo (Git.UpdateIndex.stageSymlink file sha)
		when hascontent $
			logStatus key InfoPresent
		return True
	, do
		_ <- link file key hascontent
		params <- ifM (Annex.getState Annex.force)
			( return [Param "-f"]
			, return []
			)
		Annex.Queue.addCommand "add" (params++[Param "--"]) [file]
		return True
	)
