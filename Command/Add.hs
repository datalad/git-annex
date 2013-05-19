{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Add where

import System.PosixCompat.Files

import Common.Annex
import Annex.Exception
import Command
import Types.KeySource
import Backend
import Logs.Location
import Annex.Content
import Annex.Content.Direct
import Annex.Perms
import Annex.Link
import qualified Annex
import qualified Annex.Queue
#ifndef __ANDROID__
import Utility.Touch
#endif
import Utility.FileMode
import Config
import Utility.InodeCache
import Annex.FileMatcher
import Annex.ReplaceFile

def :: [Command]
def = [notBareRepo $ command "add" paramPaths seek SectionCommon
	"add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files.
 -
 - In direct mode, it acts on any files that have changed. -}
seek :: [CommandSeek]
seek =
	[ go withFilesNotInGit
	, whenNotDirect $ go withFilesUnlocked
	, whenDirect $ go withFilesMaybeModified
	]
  where
  	go a = withValue largeFilesMatcher $ \matcher ->
		a $ \file -> ifM (checkFileMatcher matcher file <||> Annex.getState Annex.force)
			( start file
			, stop
			)

{- The add subcommand annexes a file, generating a key for it using a
 - backend, and then moving it into the annex directory and setting up
 - the symlink pointing to its content. -}
start :: FilePath -> CommandStart
start file = ifAnnexed file addpresent add
  where
	add = do
		ms <- liftIO $ catchMaybeIO $ getSymbolicLinkStatus file
		case ms of
			Nothing -> stop
			Just s
				| isSymbolicLink s || not (isRegularFile s) -> stop
				| otherwise -> do
					showStart "add" file
					next $ perform file
	addpresent (key, _) = ifM isDirect
		( ifM (goodContent key file) ( stop , add )
		, fixup key
		)
	fixup key = do
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
lockDown file = ifM (crippledFileSystem)
	( liftIO $ catchMaybeIO $ do
		cache <- genInodeCache file
		return $ KeySource
			{ keyFilename = file
			, contentLocation = file
			, inodeCache = cache
			}
	, do
		tmp <- fromRepo gitAnnexTmpDir
		createAnnexDirectory tmp
		liftIO $ catchMaybeIO $ do
			preventWrite file
			(tmpfile, h) <- openTempFile tmp (takeFileName file)
			hClose h
			nukeFile tmpfile
			createLink file tmpfile
			cache <- genInodeCache tmpfile
			return $ KeySource
				{ keyFilename = file
				, contentLocation = tmpfile
				, inodeCache = cache
				}
	)

{- Ingests a locked down file into the annex.
 -
 - In direct mode, leaves the file alone, and just updates bookkeeping
 - information.
 -}
ingest :: (Maybe KeySource) -> Annex (Maybe Key)
ingest Nothing = return Nothing
ingest (Just source) = do
	backend <- chooseBackend $ keyFilename source
	k <- genKey source backend
	cache <- liftIO $ genInodeCache $ contentLocation source
	case (cache, inodeCache source) of
		(_, Nothing) -> go k cache
		(Just newc, Just c) | compareStrong c newc -> go k cache
		_ -> failure "changed while it was being added"
  where
	go k cache = ifM isDirect ( godirect k cache , goindirect k cache )

	goindirect (Just (key, _)) _ = do
		catchAnnex (moveAnnex key $ contentLocation source)
			(undo (keyFilename source) key)
		liftIO $ nukeFile $ keyFilename source
		return $ Just key
	goindirect Nothing _ = failure "failed to generate a key"

	godirect (Just (key, _)) (Just cache) = do
		addInodeCache key cache
		finishIngestDirect key source
		return $ Just key
	godirect _ _ = failure "failed to generate a key"

	failure msg = do
		warning $ keyFilename source ++ " " ++ msg
		when (contentLocation source /= keyFilename source) $
			liftIO $ nukeFile $ contentLocation source
		return Nothing		

finishIngestDirect :: Key -> KeySource -> Annex ()
finishIngestDirect key source = do
	void $ addAssociatedFile key $ keyFilename source
	unlessM crippledFileSystem $
		liftIO $ allowWrite $ keyFilename source
	when (contentLocation source /= keyFilename source) $
		liftIO $ nukeFile $ contentLocation source

	{- Copy to any other locations using the same key. -}
	otherfs <- filter (/= keyFilename source) <$> associatedFiles key
	forM_ otherfs $
		addContentWhenNotPresent key (keyFilename source)

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
		catchAnnex (fromAnnex key file) tryharder
		logStatus key InfoMissing
	throw e
  where
	-- fromAnnex could fail if the file ownership is weird
	tryharder :: IOException -> Annex ()
	tryharder _ = do
		src <- calcRepo $ gitAnnexLocation key
		liftIO $ moveFile src file

{- Creates the symlink to the annexed content, returns the link target. -}
link :: FilePath -> Key -> Bool -> Annex String
link file key hascontent = flip catchAnnex (undo file key) $ do
	l <- inRepo $ gitAnnexLink file key
	replaceFile file $ makeAnnexLink l

#ifndef __ANDROID__
	when hascontent $ do
		-- touch the symlink to have the same mtime as the
		-- file it points to
		liftIO $ do
			mtime <- modificationTime <$> getFileStatus file
			touch file (TimeSpec mtime) False
#endif

	return l

{- Creates the symlink to the annexed content, and stages it in git.
 -
 - As long as the filesystem supports symlinks, we use
 - git add, rather than directly staging the symlink to git.
 - Using git add is best because it allows the queuing to work
 - and is faster (staging the symlink runs hash-object commands each time).
 - Also, using git add allows it to skip gitignored files, unless forced
 - to include them.
 -}
addLink :: FilePath -> Key -> Bool -> Annex ()
addLink file key hascontent = ifM (coreSymlinks <$> Annex.getGitConfig)
	( do
		_ <- link file key hascontent
		params <- ifM (Annex.getState Annex.force)
			( return [Param "-f"]
			, return []
			)
		Annex.Queue.addCommand "add" (params++[Param "--"]) [file]
	, do
		l <- link file key hascontent
		addAnnexLink l file
	)

cleanup :: FilePath -> Key -> Bool -> CommandCleanup
cleanup file key hascontent = do
	when hascontent $
		logStatus key InfoPresent
	ifM (isDirect <&&> pure hascontent)
		( do
			l <- inRepo $ gitAnnexLink file key
			stageSymlink file =<< hashSymlink l
		, addLink file key hascontent
		)
	return True
