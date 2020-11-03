{- git-annex content ingestion
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
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
	CheckGitIgnore(..),
	gitAddParams,
	addAnnexedFile,
) where

import Annex.Common
import Types.KeySource
import Types.FileMatcher
import Backend
import Annex.Content
import Annex.Perms
import Annex.Link
import Annex.MetaData
import Annex.CurrentBranch
import Annex.CheckIgnore
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
import Annex.FileMatcher

import Control.Exception (IOException)
import qualified Utility.RawFilePath as R

data LockedDown = LockedDown
	{ lockDownConfig :: LockDownConfig
	, keySource :: KeySource
	}
	deriving (Show)

data LockDownConfig = LockDownConfig
	{ lockingFile :: Bool
	-- ^ write bit removed during lock down
	, hardlinkFileTmpDir :: Maybe RawFilePath
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
	file' = toRawFilePath file

	nohardlink = withTSDelta $ liftIO . nohardlink'

	nohardlink' delta = do
		cache <- genInodeCache (toRawFilePath file) delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file'
			, contentLocation = file'
			, inodeCache = cache
			}
	
	withhardlink tmpdir = do
		when (lockingFile cfg) $
			freezeContent file
		withTSDelta $ \delta -> liftIO $ do
			(tmpfile, h) <- openTempFile (fromRawFilePath tmpdir) $
				relatedTemplate $ "ingest-" ++ takeFileName file
			hClose h
			removeWhenExistsWith removeLink tmpfile
			withhardlink' delta tmpfile
				`catchIO` const (nohardlink' delta)

	withhardlink' delta tmpfile = do
		createLink file tmpfile
		cache <- genInodeCache (toRawFilePath tmpfile) delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file'
			, contentLocation = toRawFilePath tmpfile
			, inodeCache = cache
			}

{- Ingests a locked down file into the annex. Updates the work tree and
 - index. -}
ingestAdd :: CheckGitIgnore -> MeterUpdate -> Maybe LockedDown -> Annex (Maybe Key)
ingestAdd ci meterupdate ld = ingestAdd' ci meterupdate ld Nothing

ingestAdd' :: CheckGitIgnore -> MeterUpdate -> Maybe LockedDown -> Maybe Key -> Annex (Maybe Key)
ingestAdd' _ _ Nothing _ = return Nothing
ingestAdd' ci meterupdate ld@(Just (LockedDown cfg source)) mk = do
	(mk', mic) <- ingest meterupdate ld mk
	case mk' of
		Nothing -> return Nothing
		Just k -> do
			let f = keyFilename source
			if lockingFile cfg
				then addLink ci f k mic
				else do
					mode <- liftIO $ catchMaybeIO $
						fileMode <$> R.getFileStatus (contentLocation source)
					stagePointerFile f mode =<< hashPointerFile k
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
			backend <- maybe
				(chooseBackend $ keyFilename source)
				(return . Just)
				preferredbackend
			fst <$> genKey source meterupdate backend
		Just k -> return k
	let src = contentLocation source
	ms <- liftIO $ catchMaybeIO $ R.getFileStatus src
	mcache <- maybe (pure Nothing) (liftIO . toInodeCache delta (fromRawFilePath src)) ms
	case (mcache, inodeCache source) of
		(_, Nothing) -> go k mcache ms
		(Just newc, Just c) | compareStrong c newc -> go k mcache ms
		_ -> failure "changed while it was being added"
  where
	go key mcache (Just s)
		| lockingFile cfg = golocked key mcache s
		| otherwise = gounlocked key mcache s
	go _ _ Nothing = failure "failed to generate a key"

	golocked key mcache s =
		tryNonAsync (moveAnnex key $ contentLocation source) >>= \case
			Right True -> do
				populateAssociatedFiles key source restage
				success key mcache s		
			Right False -> giveup "failed to add content to annex"
			Left e -> restoreFile (fromRawFilePath $ keyFilename source) key e

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
		genMetaData k (keyFilename source) s
		return (Just k, mcache)

	failure msg = do
		warning $ fromRawFilePath (keyFilename source) ++ " " ++ msg
		cleanCruft source
		return (Nothing, Nothing)

finishIngestUnlocked :: Key -> KeySource -> Annex ()
finishIngestUnlocked key source = do
	cleanCruft source
	finishIngestUnlocked' key source (Restage True)

finishIngestUnlocked' :: Key -> KeySource -> Restage -> Annex ()
finishIngestUnlocked' key source restage = do
	Database.Keys.addAssociatedFile key
		=<< inRepo (toTopFilePath (keyFilename source))
	populateAssociatedFiles key source restage

{- Copy to any other locations using the same key. -}
populateAssociatedFiles :: Key -> KeySource -> Restage -> Annex ()
populateAssociatedFiles key source restage = do
	obj <- calcRepo (gitAnnexLocation key)
	g <- Annex.gitRepo
	ingestedf <- flip fromTopFilePath g
		<$> inRepo (toTopFilePath (keyFilename source))
	afs <- map (`fromTopFilePath` g) <$> Database.Keys.getAssociatedFiles key
	forM_ (filter (/= ingestedf) afs) $
		populatePointerFile restage key obj

cleanCruft :: KeySource -> Annex ()
cleanCruft source = when (contentLocation source /= keyFilename source) $
	liftIO $ removeWhenExistsWith R.removeLink $ contentLocation source

-- If a worktree file was was hard linked to an annex object before,
-- modifying the file would have caused the object to have the wrong
-- content. Clean up from that.
cleanOldKeys :: RawFilePath -> Key -> Annex ()
cleanOldKeys file newkey = do
	g <- Annex.gitRepo
	topf <- inRepo (toTopFilePath file)
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
					void $ linkToAnnex key f ic
				_ -> logStatus key InfoMissing

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
restoreFile :: FilePath -> Key -> SomeException -> Annex a
restoreFile file key e = do
	whenM (inAnnex key) $ do
		liftIO $ removeWhenExistsWith removeLink file
		-- The key could be used by other files too, so leave the
		-- content in the annex, and make a copy back to the file.
		obj <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
		unlessM (liftIO $ copyFileExternal CopyTimeStamps obj file) $
			warning $ "Unable to restore content of " ++ file ++ "; it should be located in " ++ obj
		thawContent file
	throwM e

{- Creates the symlink to the annexed content, returns the link target. -}
makeLink :: RawFilePath -> Key -> Maybe InodeCache -> Annex LinkTarget
makeLink file key mcache = flip catchNonAsync (restoreFile file' key) $ do
	l <- calcRepo $ gitAnnexLink file key
	replaceWorkTreeFile file' $ makeAnnexLink l . toRawFilePath

	-- touch symlink to have same time as the original file,
	-- as provided in the InodeCache
	case mcache of
		Just c -> liftIO $ touch file (inodeCacheToMtime c) False
		Nothing -> noop

	return l
  where
	file' = fromRawFilePath file

{- Creates the symlink to the annexed content, and stages it in git.
 -
 - As long as the filesystem supports symlinks, we use
 - git add, rather than directly staging the symlink to git.
 - Using git add is best because it allows the queuing to work
 - and is faster (staging the symlink runs hash-object commands each time).
 - Also, using git add allows it to skip gitignored files, unless forced
 - to include them.
 -}
addLink :: CheckGitIgnore -> RawFilePath -> Key -> Maybe InodeCache -> Annex ()
addLink ci file key mcache = ifM (coreSymlinks <$> Annex.getGitConfig)
	( do
		_ <- makeLink file key mcache
		ps <- gitAddParams ci
		Annex.Queue.addCommand "add" (ps++[Param "--"]) [fromRawFilePath file]
	, do
		l <- makeLink file key mcache
		addAnnexLink l file
	)

{- Parameters to pass to git add, forcing addition of ignored files.
 -
 - Note that, when git add is being run on an ignored file that is already
 - checked in, CheckGitIgnore True has no effect.
 -}
gitAddParams :: CheckGitIgnore -> Annex [CommandParam]
gitAddParams (CheckGitIgnore True) = ifM (Annex.getState Annex.force)
	( return [Param "-f"]
	, return []
	)
gitAddParams (CheckGitIgnore False) = return [Param "-f"]

{- Whether a file should be added unlocked or not. Default is to not,
 - unless symlinks are not supported. annex.addunlocked can override that.
 - Also, when in an adjusted unlocked branch, always add files unlocked.
 -}
addUnlocked :: AddUnlockedMatcher -> MatchInfo -> Annex Bool
addUnlocked matcher mi =
	((not . coreSymlinks <$> Annex.getGitConfig) <||>
	 (checkAddUnlockedMatcher matcher mi) <||>
	 (maybe False isadjustedunlocked . snd <$> getCurrentBranch)
	)
  where
	isadjustedunlocked (LinkAdjustment UnlockAdjustment) = True
	isadjustedunlocked (PresenceAdjustment _ (Just UnlockAdjustment)) = True
	isadjustedunlocked _ = False

{- Adds a file to the work tree for the key, and stages it in the index.
 - The content of the key may be provided in a temp file, which will be
 - moved into place. If no content is provided, adds an annex link but does
 - not ingest the content.
 -
 - When the content of the key is not accepted into the annex, returns False.
 -}
addAnnexedFile :: CheckGitIgnore -> AddUnlockedMatcher -> RawFilePath -> Key -> Maybe RawFilePath -> Annex Bool
addAnnexedFile ci matcher file key mtmp = ifM (addUnlocked matcher mi)
	( do
		mode <- maybe
			(pure Nothing)
			(\tmp -> liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus tmp)
			mtmp
		stagePointerFile file mode =<< hashPointerFile key
		Database.Keys.addAssociatedFile key =<< inRepo (toTopFilePath file)
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
		addLink ci file key Nothing
		case mtmp of
			Just tmp -> moveAnnex key tmp
			Nothing -> return True
	)
  where
	mi = case mtmp of
		Just tmp -> MatchingFile $ FileInfo
			{ contentFile = Just tmp
			, matchFile = file
			}
		-- Provide as much info as we can without access to the
		-- file's content.
		Nothing -> MatchingInfo $ ProvidedInfo
			{ providedFilePath = file
			, providedKey = Just key
			, providedFileSize = fromMaybe 0 $
				keySize `fromKey` key
			, providedMimeType = Nothing
			, providedMimeEncoding = Nothing
			}
	
	linkunlocked mode = linkFromAnnex key file mode >>= \case
		LinkAnnexFailed -> liftIO $ writepointer mode
		_ -> return ()
	
	writepointer mode = liftIO $ writePointerFile file key mode
