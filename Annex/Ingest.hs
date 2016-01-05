{- git-annex content ingestion
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ingest (
	LockedDown(..),
	lockDown,
	ingest,
	finishIngestDirect,
	finishIngestUnlocked,
	cleanOldKeys,
	addLink,
	makeLink,
	restoreFile,
	forceParams,
) where

import Common.Annex
import Types.KeySource
import Backend
import Annex.Content
import Annex.Content.Direct
import Annex.Perms
import Annex.Link
import Annex.MetaData
import Logs.Location
import qualified Annex
import qualified Annex.Queue
import qualified Database.Keys
import Config
import Utility.InodeCache
import Annex.ReplaceFile
import Utility.Tmp
import Utility.CopyFile
import Git.FilePath
import Annex.InodeSentinal
#ifdef WITH_CLIBS
#ifndef __ANDROID__
import Utility.Touch
#endif
#endif

import Control.Exception (IOException)

data LockedDown = LockedDown
	{ lockingFile :: Bool
	, keySource :: KeySource
	}
	deriving (Show)

{- The file that's being ingested is locked down before a key is generated,
 - to prevent it from being modified in between. This lock down is not
 - perfect at best (and pretty weak at worst). For example, it does not
 - guard against files that are already opened for write by another process.
 - So, the InodeCache can be used to detect any changes that might be made
 - to the file after it was locked down.
 -
 - When possible, the file is hard linked to a temp directory. This guards
 - against some changes, like deletion or overwrite of the file, and
 - allows lsof checks to be done more efficiently when adding a lot of files.
 -
 - If lockingfile is True, the file is going to be added in locked mode.
 - So, its write bit is removed as part of the lock down.
 -
 - Lockdown can fail if a file gets deleted, and Nothing will be returned.
 -}
lockDown :: Bool -> FilePath -> Annex (Maybe LockedDown)
lockDown lockingfile file = either 
		(\e -> warning (show e) >> return Nothing)
		(return . Just)
	=<< lockDown' lockingfile file

lockDown' :: Bool -> FilePath -> Annex (Either IOException LockedDown)
lockDown' lockingfile file = ifM crippledFileSystem
	( withTSDelta $ liftIO . tryIO . nohardlink
	, tryIO $ do
		tmp <- fromRepo gitAnnexTmpMiscDir
		createAnnexDirectory tmp
		when lockingfile $
			freezeContent file
		withTSDelta $ \delta -> liftIO $ do
			(tmpfile, h) <- openTempFile tmp $
				relatedTemplate $ takeFileName file
			hClose h
			nukeFile tmpfile
			withhardlink delta tmpfile `catchIO` const (nohardlink delta)
	)
  where
	nohardlink delta = do
		cache <- genInodeCache file delta
		return $ LockedDown lockingfile $ KeySource
			{ keyFilename = file
			, contentLocation = file
			, inodeCache = cache
			}
	withhardlink delta tmpfile = do
		createLink file tmpfile
		cache <- genInodeCache tmpfile delta
		return $ LockedDown lockingfile $ KeySource
			{ keyFilename = file
			, contentLocation = tmpfile
			, inodeCache = cache
			}

{- Ingests a locked down file into the annex.
 -
 - The file may be added to the git repository as a locked or an unlocked
 - file. When unlocked, the work tree file is left alone. When locked, 
 - the work tree file is deleted, in preparation for adding the symlink.
 -}
ingest :: Maybe LockedDown -> Annex (Maybe Key, Maybe InodeCache)
ingest Nothing = return (Nothing, Nothing)
ingest (Just (LockedDown lockingfile source)) = withTSDelta $ \delta -> do
	backend <- chooseBackend $ keyFilename source
	k <- genKey source backend
	let src = contentLocation source
	ms <- liftIO $ catchMaybeIO $ getFileStatus src
	mcache <- maybe (pure Nothing) (liftIO . toInodeCache delta src) ms
	case (mcache, inodeCache source) of
		(_, Nothing) -> go k mcache ms
		(Just newc, Just c) | compareStrong c newc -> go k mcache ms
		_ -> failure "changed while it was being added"
  where
	go (Just (key, _)) mcache (Just s)
		| lockingfile = golocked key mcache s
		| otherwise = ifM isDirect
			( godirect key mcache s
			, gounlocked key mcache s
			)
	go _ _ _ = failure "failed to generate a key"

	golocked key mcache s = do
		catchNonAsync (moveAnnex key $ contentLocation source)
			(restoreFile (keyFilename source) key)
		liftIO $ nukeFile $ keyFilename source
		populateAssociatedFiles key source
		success key mcache s

	gounlocked key (Just cache) s = do
		-- Remove temp directory hard link first because
		-- linkToAnnex falls back to copying if a file
		-- already has a hard link.
		cleanCruft source
		cleanOldKeys (keyFilename source) key
		r <- linkToAnnex key (keyFilename source) (Just cache)
		case r of
			LinkAnnexFailed -> failure "failed to link to annex"
			_ -> do
				finishIngestUnlocked' key source
				success key (Just cache) s
	gounlocked _ _ _ = failure "failed statting file"

	godirect key (Just cache) s = do
		addInodeCache key cache
		finishIngestDirect key source
		success key (Just cache) s
	godirect _ _ _ = failure "failed statting file"

	success k mcache s = do
		genMetaData k (keyFilename source) s
		return (Just k, mcache)

	failure msg = do
		warning $ keyFilename source ++ " " ++ msg
		cleanCruft source
		return (Nothing, Nothing)

finishIngestDirect :: Key -> KeySource -> Annex ()
finishIngestDirect key source = do
	void $ addAssociatedFile key $ keyFilename source
	cleanCruft source

	{- Copy to any other locations using the same key. -}
	otherfs <- filter (/= keyFilename source) <$> associatedFiles key
	forM_ otherfs $
		addContentWhenNotPresent key (keyFilename source)

finishIngestUnlocked :: Key -> KeySource -> Annex ()
finishIngestUnlocked key source = do
	cleanCruft source
	finishIngestUnlocked' key source

finishIngestUnlocked' :: Key -> KeySource -> Annex ()
finishIngestUnlocked' key source = do
	Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath (keyFilename source))
	populateAssociatedFiles key source

{- Copy to any other locations using the same key. -}
populateAssociatedFiles :: Key -> KeySource -> Annex ()
populateAssociatedFiles key source = do
	obj <- calcRepo (gitAnnexLocation key)
	g <- Annex.gitRepo
	ingestedf <- flip fromTopFilePath g
		<$> inRepo (toTopFilePath (keyFilename source))
	afs <- map (`fromTopFilePath` g) <$> Database.Keys.getAssociatedFiles key
	forM_ (filter (/= ingestedf) afs) $
		populatePointerFile key obj

cleanCruft :: KeySource -> Annex ()
cleanCruft source = when (contentLocation source /= keyFilename source) $
	liftIO $ nukeFile $ contentLocation source

-- If a worktree file was was hard linked to an annex object before,
-- modifying the file would have caused the object to have the wrong
-- content. Clean up from that.
cleanOldKeys :: FilePath -> Key -> Annex ()
cleanOldKeys file newkey = do
	g <- Annex.gitRepo
	ingestedf <- flip fromTopFilePath g <$> inRepo (toTopFilePath file)
	topf <- inRepo (toTopFilePath file)
	oldkeys <- filter (/= newkey)
		<$> Database.Keys.getAssociatedKey topf
	forM_ oldkeys $ \key -> do
		obj <- calcRepo (gitAnnexLocation key)
		caches <- Database.Keys.getInodeCaches key
		unlessM (sameInodeCache obj caches) $ do
			unlinkAnnex key
			fs <- filter (/= ingestedf)
				. map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			fs' <- filterM (`sameInodeCache` caches) fs
			case fs' of
				-- If linkToAnnex fails, the associated 
				-- file with the content is still present,
				-- so no need for any recovery.
				(f:_) -> do
					ic <- withTSDelta (liftIO . genInodeCache f)
					void $ linkToAnnex key f ic
				_ -> logStatus key InfoMissing

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
restoreFile :: FilePath -> Key -> SomeException -> Annex a
restoreFile file key e = do
	whenM (inAnnex key) $ do
		liftIO $ nukeFile file
		-- The key could be used by other files too, so leave the
		-- content in the annex, and make a copy back to the file.
		obj <- calcRepo $ gitAnnexLocation key
		unlessM (liftIO $ copyFileExternal CopyTimeStamps obj file) $
			warning $ "Unable to restore content of " ++ file ++ "; it should be located in " ++ obj
		thawContent file
	throwM e

{- Creates the symlink to the annexed content, returns the link target. -}
makeLink :: FilePath -> Key -> Maybe InodeCache -> Annex String
makeLink file key mcache = flip catchNonAsync (restoreFile file key) $ do
	l <- calcRepo $ gitAnnexLink file key
	replaceFile file $ makeAnnexLink l

	-- touch symlink to have same time as the original file,
	-- as provided in the InodeCache
	case mcache of
#if defined(WITH_CLIBS) && ! defined(__ANDROID__)
		Just c -> liftIO $ touch file (TimeSpec $ inodeCacheToMtime c) False
#else
		Just _ -> noop
#endif
		Nothing -> noop

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
		_ <- makeLink file key mcache
		ps <- forceParams
		Annex.Queue.addCommand "add" (ps++[Param "--"]) [file]
	, do
		l <- makeLink file key mcache
		addAnnexLink l file
	)

{- Parameters to pass to git add, forcing addition of ignored files. -}
forceParams :: Annex [CommandParam]
forceParams = ifM (Annex.getState Annex.force)
	( return [Param "-f"]
	, return []
	)
