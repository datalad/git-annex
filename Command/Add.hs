{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Add where

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
import Annex.MetaData
import qualified Annex
import qualified Annex.Queue
#ifdef WITH_CLIBS
#ifndef __ANDROID__
import Utility.Touch
#endif
#endif
import Config
import Utility.InodeCache
import Annex.FileMatcher
import Annex.ReplaceFile
import Utility.Tmp

def :: [Command]
def = [notBareRepo $ command "add" paramPaths seek SectionCommon
	"add files to annex"]

{- Add acts on both files not checked into git yet, and unlocked files.
 -
 - In direct mode, it acts on any files that have changed. -}
seek :: CommandSeek
seek ps = do
	matcher <- largeFilesMatcher
	let go a = flip a ps $ \file -> ifM (checkFileMatcher matcher file <||> Annex.getState Annex.force)
		( start file
		, stop
		)
	go withFilesNotInGit
	ifM isDirect
		( go withFilesMaybeModified
		, go withFilesUnlocked
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
		next $ next $ cleanup file key Nothing =<< inAnnex key

{- The file that's being added is locked down before a key is generated,
 - to prevent it from being modified in between. This lock down is not
 - perfect at best (and pretty weak at worst). For example, it does not
 - guard against files that are already opened for write by another process.
 - So a KeySource is returned. Its inodeCache can be used to detect any
 - changes that might be made to the file after it was locked down.
 -
 - When possible, the file is hard linked to a temp directory. This guards
 - against some changes, like deletion or overwrite of the file, and
 - allows lsof checks to be done more efficiently when adding a lot of files.
 -
 - Lockdown can fail if a file gets deleted, and Nothing will be returned.
 -}
lockDown :: FilePath -> Annex (Maybe KeySource)
lockDown file = ifM crippledFileSystem
	( liftIO $ catchMaybeIO nohardlink
	, do
		tmp <- fromRepo gitAnnexTmpDir
		createAnnexDirectory tmp
		eitherToMaybe <$> tryAnnexIO (go tmp)
	)
  where
	{- In indirect mode, the write bit is removed from the file as part
	 - of lock down to guard against further writes, and because objects
	 - in the annex have their write bit disabled anyway.
	 -
	 - Freezing the content early also lets us fail early when
	 - someone else owns the file.
	 -
	 - This is not done in direct mode, because files there need to
	 - remain writable at all times.
	-}
  	go tmp = do
		unlessM isDirect $
			freezeContent file
		liftIO $ do
			(tmpfile, h) <- openTempFile tmp $
				relatedTemplate $ takeFileName file
			hClose h
			nukeFile tmpfile
			withhardlink tmpfile `catchIO` const nohardlink
  	nohardlink = do
		cache <- genInodeCache file
		return KeySource
			{ keyFilename = file
			, contentLocation = file
			, inodeCache = cache
			}
	withhardlink tmpfile = do
		createLink file tmpfile
		cache <- genInodeCache tmpfile
		return KeySource
			{ keyFilename = file
			, contentLocation = tmpfile
			, inodeCache = cache
			}

{- Ingests a locked down file into the annex.
 -
 - In direct mode, leaves the file alone, and just updates bookkeeping
 - information.
 -}
ingest :: Maybe KeySource -> Annex (Maybe Key, Maybe InodeCache)
ingest Nothing = return (Nothing, Nothing)
ingest (Just source) = do
	backend <- chooseBackend $ keyFilename source
	k <- genKey source backend
	ms <- liftIO $ catchMaybeIO $ getFileStatus $ contentLocation source
	let mcache = toInodeCache =<< ms
	case (mcache, inodeCache source) of
		(_, Nothing) -> go k mcache ms
		(Just newc, Just c) | compareStrong c newc -> go k mcache ms
		_ -> failure "changed while it was being added"
  where
	go k mcache ms = ifM isDirect
		( godirect k mcache ms
		, goindirect k mcache ms
		)

	goindirect (Just (key, _)) mcache ms = do
		catchAnnex (moveAnnex key $ contentLocation source)
			(undo (keyFilename source) key)
		maybe noop (genMetaData key (keyFilename source)) ms
		liftIO $ nukeFile $ keyFilename source
		return $ (Just key, mcache)
	goindirect _ _ _ = failure "failed to generate a key"

	godirect (Just (key, _)) (Just cache) ms = do
		addInodeCache key cache
		maybe noop (genMetaData key (keyFilename source)) ms
		finishIngestDirect key source
		return $ (Just key, Just cache)
	godirect _ _ _ = failure "failed to generate a key"

	failure msg = do
		warning $ keyFilename source ++ " " ++ msg
		when (contentLocation source /= keyFilename source) $
			liftIO $ nukeFile $ contentLocation source
		return (Nothing, Nothing)

finishIngestDirect :: Key -> KeySource -> Annex ()
finishIngestDirect key source = do
	void $ addAssociatedFile key $ keyFilename source
	when (contentLocation source /= keyFilename source) $
		liftIO $ nukeFile $ contentLocation source

	{- Copy to any other locations using the same key. -}
	otherfs <- filter (/= keyFilename source) <$> associatedFiles key
	forM_ otherfs $
		addContentWhenNotPresent key (keyFilename source)

perform :: FilePath -> CommandPerform
perform file = lockDown file >>= ingest >>= go
  where
  	go (Just key, cache) = next $ cleanup file key cache True
	go (Nothing, _) = stop

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
undo :: FilePath -> Key -> IOException -> Annex a
undo file key e = do
	whenM (inAnnex key) $ do
		liftIO $ nukeFile file
		catchAnnex (fromAnnex key file) tryharder
		logStatus key InfoMissing
	throwAnnex e
  where
	-- fromAnnex could fail if the file ownership is weird
	tryharder :: IOException -> Annex ()
	tryharder _ = do
		src <- calcRepo $ gitAnnexLocation key
		liftIO $ moveFile src file

{- Creates the symlink to the annexed content, returns the link target. -}
link :: FilePath -> Key -> Maybe InodeCache -> Annex String
link file key mcache = flip catchAnnex (undo file key) $ do
	l <- inRepo $ gitAnnexLink file key
	replaceFile file $ makeAnnexLink l

#ifdef WITH_CLIBS
#ifndef __ANDROID__
	-- touch symlink to have same time as the original file,
	-- as provided in the InodeCache
	case mcache of
		Just c -> liftIO $ touch file (TimeSpec $ inodeCacheToMtime c) False
		Nothing -> noop
#endif
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
addLink :: FilePath -> Key -> Maybe InodeCache -> Annex ()
addLink file key mcache = ifM (coreSymlinks <$> Annex.getGitConfig)
	( do
		_ <- link file key mcache
		params <- ifM (Annex.getState Annex.force)
			( return [Param "-f"]
			, return []
			)
		Annex.Queue.addCommand "add" (params++[Param "--"]) [file]
	, do
		l <- link file key mcache
		addAnnexLink l file
	)

cleanup :: FilePath -> Key -> Maybe InodeCache -> Bool -> CommandCleanup
cleanup file key mcache hascontent = do
	ifM (isDirect <&&> pure hascontent)
		( do
			l <- inRepo $ gitAnnexLink file key
			stageSymlink file =<< hashSymlink l
		, addLink file key mcache
		)
	when hascontent $
		logStatus key InfoPresent
	return True
