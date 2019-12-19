{- git-annex content ingestion
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Ingest (
	LockedDown(..),
	LockDownConfig(..),
	lockDown,
	ingestAdd,
	ingestAdd',
	ingest,
	ingest',
	finishIngestUnlocked,
	cleanOldKeys,
	addLink,
	makeLink,
	addUnlocked,
	restoreFile,
	forceParams,
	addAnnexedFile,
) where

import Annex.Common
import Types.KeySource
import Backend
import Annex.Content
import Annex.Perms
import Annex.Link
import Annex.MetaData
import Annex.CurrentBranch
import Logs.Location
import qualified Annex
import qualified Annex.Queue
import qualified Database.Keys
import Config
import Utility.InodeCache
import Annex.ReplaceFile
import Utility.Tmp
import Utility.CopyFile
import Utility.Touch
import Utility.Metered
import Git.FilePath
import Annex.InodeSentinal
import Annex.AdjustedBranch

import Control.Exception (IOException)

data LockedDown = LockedDown
	{ lockDownConfig :: LockDownConfig
	, keySource :: KeySource
	}
	deriving (Show)

data LockDownConfig = LockDownConfig
	{ lockingFile :: Bool
	-- ^ write bit removed during lock down
	, hardlinkFileTmpDir :: Maybe FilePath
	-- ^ hard link to temp directory
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
 - Lockdown can fail if a file gets deleted, and Nothing will be returned.
 -}
lockDown :: LockDownConfig -> FilePath -> Annex (Maybe LockedDown)
lockDown cfg file = either 
		(\e -> warning (show e) >> return Nothing)
		(return . Just)
	=<< lockDown' cfg file

lockDown' :: LockDownConfig -> FilePath -> Annex (Either IOException LockedDown)
lockDown' cfg file = tryIO $ ifM crippledFileSystem
	( nohardlink
	, case hardlinkFileTmpDir cfg of
		Nothing -> nohardlink
		Just tmpdir -> withhardlink tmpdir
	)
  where
	nohardlink = withTSDelta $ liftIO . nohardlink'

	nohardlink' delta = do
		cache <- genInodeCache (toRawFilePath file) delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file
			, contentLocation = file
			, inodeCache = cache
			}
	
	withhardlink tmpdir = do
		when (lockingFile cfg) $
			freezeContent file
		withTSDelta $ \delta -> liftIO $ do
			(tmpfile, h) <- openTempFile tmpdir $
				relatedTemplate $ "ingest-" ++ takeFileName file
			hClose h
			nukeFile tmpfile
			withhardlink' delta tmpfile
				`catchIO` const (nohardlink' delta)

	withhardlink' delta tmpfile = do
		createLink file tmpfile
		cache <- genInodeCache (toRawFilePath tmpfile) delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file
			, contentLocation = tmpfile
			, inodeCache = cache
			}

{- Ingests a locked down file into the annex. Updates the work tree and
 - index. -}
ingestAdd :: MeterUpdate -> Maybe LockedDown -> Annex (Maybe Key)
ingestAdd meterupdate ld = ingestAdd' meterupdate ld Nothing

ingestAdd' :: MeterUpdate -> Maybe LockedDown -> Maybe Key -> Annex (Maybe Key)
ingestAdd' _ Nothing _ = return Nothing
ingestAdd' meterupdate ld@(Just (LockedDown cfg source)) mk = do
	(mk', mic) <- ingest meterupdate ld mk
	case mk' of
		Nothing -> return Nothing
		Just k -> do
			let f = keyFilename source
			if lockingFile cfg
				then addLink f k mic
				else do
					mode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus (contentLocation source)
					stagePointerFile (toRawFilePath f) mode =<< hashPointerFile k
			return (Just k)

{- Ingests a locked down file into the annex. Does not update the working
 - tree or the index. -}
ingest :: MeterUpdate -> Maybe LockedDown -> Maybe Key -> Annex (Maybe Key, Maybe InodeCache)
ingest meterupdate ld mk = ingest' Nothing meterupdate ld mk (Restage True)

ingest' :: Maybe Backend -> MeterUpdate -> Maybe LockedDown -> Maybe Key -> Restage -> Annex (Maybe Key, Maybe InodeCache)
ingest' _ _ Nothing _ _ = return (Nothing, Nothing)
ingest' preferredbackend meterupdate (Just (LockedDown cfg source)) mk restage = withTSDelta $ \delta -> do
	k <- case mk of
		Nothing -> do
			backend <- maybe (chooseBackend $ keyFilename source) (return . Just) preferredbackend
			fmap fst <$> genKey source meterupdate backend
		Just k -> return (Just k)
	let src = contentLocation source
	ms <- liftIO $ catchMaybeIO $ getFileStatus src
	mcache <- maybe (pure Nothing) (liftIO . toInodeCache delta src) ms
	case (mcache, inodeCache source) of
		(_, Nothing) -> go k mcache ms
		(Just newc, Just c) | compareStrong c newc -> go k mcache ms
		_ -> failure "changed while it was being added"
  where
	go (Just key) mcache (Just s)
		| lockingFile cfg = golocked key mcache s
		| otherwise = gounlocked key mcache s
	go _ _ _ = failure "failed to generate a key"

	golocked key mcache s =
		tryNonAsync (moveAnnex key $ contentLocation source) >>= \case
			Right True -> do
				populateAssociatedFiles key source restage
				success key mcache s		
			Right False -> giveup "failed to add content to annex"
			Left e -> restoreFile (keyFilename source) key e

	gounlocked key (Just cache) s = do
		-- Remove temp directory hard link first because
		-- linkToAnnex falls back to copying if a file
		-- already has a hard link.
		cleanCruft source
		cleanOldKeys (keyFilename source) key
		linkToAnnex key (keyFilename source) (Just cache) >>= \case
			LinkAnnexFailed -> failure "failed to link to annex"
			_ -> do
				finishIngestUnlocked' key source restage
				success key (Just cache) s
	gounlocked _ _ _ = failure "failed statting file"

	success k mcache s = do
		genMetaData k (toRawFilePath (keyFilename source)) s
		return (Just k, mcache)

	failure msg = do
		warning $ keyFilename source ++ " " ++ msg
		cleanCruft source
		return (Nothing, Nothing)

finishIngestUnlocked :: Key -> KeySource -> Annex ()
finishIngestUnlocked key source = do
	cleanCruft source
	finishIngestUnlocked' key source (Restage True)

finishIngestUnlocked' :: Key -> KeySource -> Restage -> Annex ()
finishIngestUnlocked' key source restage = do
	Database.Keys.addAssociatedFile key
		=<< inRepo (toTopFilePath (toRawFilePath (keyFilename source)))
	populateAssociatedFiles key source restage

{- Copy to any other locations using the same key. -}
populateAssociatedFiles :: Key -> KeySource -> Restage -> Annex ()
populateAssociatedFiles key source restage = do
	obj <- calcRepo (gitAnnexLocation key)
	g <- Annex.gitRepo
	ingestedf <- flip fromTopFilePath g
		<$> inRepo (toTopFilePath (toRawFilePath (keyFilename source)))
	afs <- map (`fromTopFilePath` g) <$> Database.Keys.getAssociatedFiles key
	forM_ (filter (/= ingestedf) afs) $
		populatePointerFile restage key obj

cleanCruft :: KeySource -> Annex ()
cleanCruft source = when (contentLocation source /= keyFilename source) $
	liftIO $ nukeFile $ contentLocation source

-- If a worktree file was was hard linked to an annex object before,
-- modifying the file would have caused the object to have the wrong
-- content. Clean up from that.
cleanOldKeys :: FilePath -> Key -> Annex ()
cleanOldKeys file newkey = do
	g <- Annex.gitRepo
	topf <- inRepo (toTopFilePath (toRawFilePath file))
	ingestedf <- fromRepo $ fromTopFilePath topf
	oldkeys <- filter (/= newkey)
		<$> Database.Keys.getAssociatedKey topf
	forM_ oldkeys $ \key ->
		unlessM (isUnmodified key =<< calcRepo (gitAnnexLocation key)) $ do
			caches <- Database.Keys.getInodeCaches key
			unlinkAnnex key
			fs <- filter (/= ingestedf)
				. map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			filterM (`sameInodeCache` caches) fs >>= \case
				-- If linkToAnnex fails, the associated 
				-- file with the content is still present,
				-- so no need for any recovery.
				(f:_) -> do
					ic <- withTSDelta (liftIO . genInodeCache f)
					void $ linkToAnnex key (fromRawFilePath f) ic
				_ -> logStatus key InfoMissing

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
restoreFile :: FilePath -> Key -> SomeException -> Annex a
restoreFile file key e = do
	whenM (inAnnex key) $ do
		liftIO $ nukeFile file
		-- The key could be used by other files too, so leave the
		-- content in the annex, and make a copy back to the file.
		obj <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
		unlessM (liftIO $ copyFileExternal CopyTimeStamps obj file) $
			warning $ "Unable to restore content of " ++ file ++ "; it should be located in " ++ obj
		thawContent file
	throwM e

{- Creates the symlink to the annexed content, returns the link target. -}
makeLink :: FilePath -> Key -> Maybe InodeCache -> Annex String
makeLink file key mcache = flip catchNonAsync (restoreFile file key) $ do
	l <- calcRepo $ gitAnnexLink file key
	replaceFile file $ makeAnnexLink l . toRawFilePath

	-- touch symlink to have same time as the original file,
	-- as provided in the InodeCache
	case mcache of
		Just c -> liftIO $ touch file (inodeCacheToMtime c) False
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
		addAnnexLink l (toRawFilePath file)
	)

{- Parameters to pass to git add, forcing addition of ignored files. -}
forceParams :: Annex [CommandParam]
forceParams = ifM (Annex.getState Annex.force)
	( return [Param "-f"]
	, return []
	)

{- Whether a file should be added unlocked or not. Default is to not,
 - unless symlinks are not supported. annex.addunlocked can override that.
 - Also, when in an adjusted unlocked branch, always add files unlocked.
 -}
addUnlocked :: Annex Bool
addUnlocked =
	((not . coreSymlinks <$> Annex.getGitConfig) <||>
	 (annexAddUnlocked <$> Annex.getGitConfig) <||>
	 (maybe False isadjustedunlocked . snd <$> getCurrentBranch)
	)
  where
	isadjustedunlocked (LinkAdjustment UnlockAdjustment) = True
	isadjustedunlocked (PresenceAdjustment _ (Just UnlockAdjustment)) = True
	isadjustedunlocked _ = False

{- Adds a file to the work tree for the key, and stages it in the index.
 - The content of the key may be provided in a temp file, which will be
 - moved into place.
 -
 - When the content of the key is not accepted into the annex, returns False.
 -}
addAnnexedFile :: FilePath -> Key -> Maybe FilePath -> Annex Bool
addAnnexedFile file key mtmp = ifM addUnlocked
	( do
		mode <- maybe
			(pure Nothing)
			(\tmp -> liftIO $ catchMaybeIO $ fileMode <$> getFileStatus tmp)
			mtmp
		stagePointerFile (toRawFilePath file) mode =<< hashPointerFile key
		Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath (toRawFilePath file))
		case mtmp of
			Just tmp -> ifM (moveAnnex key tmp)
				( linkunlocked mode >> return True
				, writepointer mode >> return False
				)
			Nothing -> ifM (inAnnex key)
				( linkunlocked mode >> return True
				, writepointer mode >> return True
				)
	, do
		addLink file key Nothing
		case mtmp of
			Just tmp -> moveAnnex key tmp
			Nothing -> return True
	)
  where
	linkunlocked mode = linkFromAnnex key file mode >>= \case
		LinkAnnexFailed -> liftIO $
			writePointerFile (toRawFilePath file) key mode
		_ -> return ()
	writepointer mode = liftIO $ writePointerFile (toRawFilePath file) key mode
