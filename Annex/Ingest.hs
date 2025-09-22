{- git-annex content ingestion
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.Ingest (
	LockedDown(..),
	LockDownConfig(..),
	lockDown,
	checkLockedDownWritePerms,
	ingestAdd,
	ingestAdd',
	ingest,
	ingest',
	finishIngestUnlocked,
	cleanOldKeys,
	addSymlink,
	genSymlink,
	makeLink,
	addUnlocked,
	CheckGitIgnore(..),
	gitAddParams,
	addAnnexedFile,
	addingExistingLink,
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
import qualified Git
import qualified Annex
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
import qualified Utility.RawFilePath as R

import System.PosixCompat.Files (fileMode)

data LockedDown = LockedDown
	{ lockDownConfig :: LockDownConfig
	, keySource :: KeySource
	}
	deriving (Show)

data LockDownConfig = LockDownConfig
	{ lockingFile :: Bool
	-- ^ write bit removed during lock down
	, hardlinkFileTmpDir :: Maybe OsPath
	-- ^ hard link to temp directory
	, checkWritePerms :: Bool
	-- ^ check that write perms are successfully removed
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
 - Lockdown can fail if a file gets deleted, or if it's unable to remove
 - write permissions, and Nothing will be returned.
 -}
lockDown :: LockDownConfig-> OsPath -> Annex (Maybe LockedDown)
lockDown cfg file = either 
		(\e -> warning (UnquotedString (show e)) >> return Nothing)
		(return . Just)
	=<< lockDown' cfg file

lockDown' :: LockDownConfig -> OsPath -> Annex (Either SomeException LockedDown)
lockDown' cfg file = tryNonAsync $ ifM crippledFileSystem
	( nohardlink
	, case hardlinkFileTmpDir cfg of
		Nothing -> nohardlink
		Just tmpdir -> withhardlink tmpdir
	)
  where
	nohardlink = do
		setperms
		withTSDelta $ liftIO . nohardlink'

	nohardlink' delta = do
		cache <- genInodeCache file delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file
			, contentLocation = file
			, inodeCache = cache
			}
	
	withhardlink tmpdir = do
		setperms
		withTSDelta $ \delta -> liftIO $ do
			(tmpfile, h) <- openTmpFileIn tmpdir $
				relatedTemplate $ fromOsPath $
					literalOsPath "ingest-" <> takeFileName file
			hClose h
			removeWhenExistsWith removeFile tmpfile
			withhardlink' delta tmpfile
				`catchIO` const (nohardlink' delta)

	withhardlink' delta tmpfile = do
		R.createLink (fromOsPath file) (fromOsPath tmpfile)
		cache <- genInodeCache tmpfile delta
		return $ LockedDown cfg $ KeySource
			{ keyFilename = file
			, contentLocation = tmpfile
			, inodeCache = cache
			}
		
	setperms = when (lockingFile cfg) $ do
		freezeContent file
		when (checkWritePerms cfg) $ do
			qp <- coreQuotePath <$> Annex.getGitConfig
			maybe noop (giveup . decodeBS . quote qp)
				=<< checkLockedDownWritePerms file file

checkLockedDownWritePerms :: OsPath -> OsPath -> Annex (Maybe StringContainingQuotedPath)
checkLockedDownWritePerms file displayfile = checkContentWritePerm file >>= return . \case
	Just False -> Just $ "Unable to remove all write permissions from "
		<> QuotedPath displayfile
		<> " -- perhaps it has an xattr or ACL set."
	_ -> Nothing

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
				then addSymlink f k mic
				else do
					mode <- liftIO $ catchMaybeIO $
						fileMode <$> R.getFileStatus
							(fromOsPath (contentLocation source))
					stagePointerFile f mode =<< hashPointerFile k
			return (Just k)

{- Ingests a locked down file into the annex. Does not update the working
 - tree or the index. -}
ingest :: MeterUpdate -> Maybe LockedDown -> Maybe Key -> Annex (Maybe Key, Maybe InodeCache)
ingest meterupdate ld mk = ingest' Nothing meterupdate ld mk QueueRestage

ingest' :: Maybe Backend -> MeterUpdate -> Maybe LockedDown -> Maybe Key -> Restage -> Annex (Maybe Key, Maybe InodeCache)
ingest' _ _ Nothing _ _ = return (Nothing, Nothing)
ingest' preferredbackend meterupdate (Just (LockedDown cfg source)) mk restage = withTSDelta $ \delta -> do
	k <- case mk of
		Nothing -> do
			backend <- maybe
				(chooseBackend $ keyFilename source)
				return
				preferredbackend
			fst <$> genKey source meterupdate backend
		Just k -> return k
	let src = contentLocation source
	ms <- liftIO $ catchMaybeIO $ R.getFileStatus (fromOsPath src)
	mcache <- maybe (pure Nothing) (liftIO . toInodeCache delta src) ms
	case (mcache, inodeCache source) of
		(_, Nothing) -> go k mcache
		(Just newc, Just c) | compareStrong c newc -> go k mcache
		_ -> failure "changed while it was being added"
  where
	go key mcache
		| lockingFile cfg = golocked key mcache
		| otherwise = gounlocked key mcache

	golocked key mcache =
		tryNonAsync (moveAnnex key (contentLocation source)) >>= \case
			Right True -> success key mcache
			Right False -> giveup "failed to add content to annex"
			Left e -> restoreFile (keyFilename source) key e

	gounlocked key (Just cache) = do
		-- Remove temp directory hard link first because
		-- linkToAnnex falls back to copying if a file
		-- already has a hard link.
		cleanCruft source
		cleanOldKeys (keyFilename source) key
		linkToAnnex key (keyFilename source) (Just cache) >>= \case
			LinkAnnexFailed -> failure "failed to link to annex"
			lar -> do
				finishIngestUnlocked' key source restage (Just lar)
				success key (Just cache)
	gounlocked _ _ = failure "failed statting file"

	success k mcache = do
		genMetaData k (keyFilename source) (fmap inodeCacheToMtime mcache)
		return (Just k, mcache)

	failure msg = do
		warning $ QuotedPath (keyFilename source) <> " " <> UnquotedString msg
		cleanCruft source
		return (Nothing, Nothing)

finishIngestUnlocked :: Key -> KeySource -> Annex ()
finishIngestUnlocked key source = do
	cleanCruft source
	finishIngestUnlocked' key source QueueRestage Nothing

finishIngestUnlocked' :: Key -> KeySource -> Restage -> Maybe LinkAnnexResult -> Annex ()
finishIngestUnlocked' key source restage lar = do
	Database.Keys.addAssociatedFile key
		=<< inRepo (toTopFilePath (keyFilename source))
	populateUnlockedFiles key source restage lar

{- Copy to any other unlocked files using the same key.
 -
 - When linkToAnnex did not have to do anything, the object file
 - was already present, and so other unlocked files are already populated,
 - and nothing needs to be done here.
 -}
populateUnlockedFiles :: Key -> KeySource -> Restage -> Maybe LinkAnnexResult -> Annex ()
populateUnlockedFiles _ _ _ (Just LinkAnnexNoop) = return ()
populateUnlockedFiles key source restage _ = do
	obj <- calcRepo (gitAnnexLocation key)
	g <- Annex.gitRepo
	ingestedf <- flip fromTopFilePath g
		<$> inRepo (toTopFilePath (keyFilename source))
	afs <- map (`fromTopFilePath` g) <$> Database.Keys.getAssociatedFiles key
	forM_ (filter (/= ingestedf) afs) $
		populatePointerFile restage key obj

cleanCruft :: KeySource -> Annex ()
cleanCruft source = when (contentLocation source /= keyFilename source) $
	liftIO $ removeWhenExistsWith removeFile $ contentLocation source

-- If a worktree file was was hard linked to an annex object before,
-- modifying the file would have caused the object to have the wrong
-- content. Clean up from that.
cleanOldKeys :: OsPath -> Key -> Annex ()
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
				_ -> logStatus NoLiveUpdate key InfoMissing

{- On error, put the file back so it doesn't seem to have vanished.
 - This can be called before or after the symlink is in place. -}
restoreFile :: OsPath -> Key -> SomeException -> Annex a
restoreFile file key e = do
	whenM (inAnnex key) $ do
		liftIO $ removeWhenExistsWith removeFile file
		-- The key could be used by other files too, so leave the
		-- content in the annex, and make a copy back to the file.
		obj <- calcRepo (gitAnnexLocation key)
		unlessM (liftIO $ copyFileExternal CopyTimeStamps obj file) $
			warning $ "Unable to restore content of " <> QuotedPath file <> "; it should be located in " <> QuotedPath obj
		thawContent file
	throwM e

{- Creates the symlink to the annexed content, returns the link target. -}
makeLink :: OsPath -> Key -> Maybe InodeCache -> Annex LinkTarget
makeLink file key mcache = flip catchNonAsync (restoreFile file key) $ do
	l <- fromOsPath <$> calcRepo (gitAnnexLink file key)
	replaceWorkTreeFile file $ makeAnnexLink l

	-- touch symlink to have same time as the original file,
	-- as provided in the InodeCache
	case mcache of
		Just c -> liftIO $
			touch (fromOsPath file) (inodeCacheToMtime c) False
		Nothing -> noop

	return l

{- Creates the symlink to the annexed content, and stages it in git. -}
addSymlink :: OsPath -> Key -> Maybe InodeCache -> Annex ()
addSymlink file key mcache = stageSymlink file =<< genSymlink file key mcache

genSymlink :: OsPath -> Key -> Maybe InodeCache -> Annex Git.Sha
genSymlink file key mcache = do
	linktarget <- makeLink file key mcache
	hashSymlink linktarget

{- Parameters to pass to git add, forcing addition of ignored files.
 -
 - Note that, when git add is being run on an ignored file that is already
 - checked in, CheckGitIgnore True has no effect.
 -}
gitAddParams :: CheckGitIgnore -> Annex [CommandParam]
gitAddParams (CheckGitIgnore True) = ifM (Annex.getRead Annex.force)
	( return [Param "-f"]
	, return []
	)
gitAddParams (CheckGitIgnore False) = return [Param "-f"]

{- Whether a file should be added unlocked or not. Default is to not,
 - unless symlinks are not supported. annex.addunlocked can override that.
 - Also, when in an adjusted branch that unlocked files, always add files
 - unlocked.
 -}
addUnlocked :: AddUnlockedMatcher -> MatchInfo -> Bool -> Annex Bool
addUnlocked matcher mi contentpresent =
	((not . coreSymlinks <$> Annex.getGitConfig) <||>
	 (checkAddUnlockedMatcher NoLiveUpdate matcher mi) <||>
	 (maybe False go . snd <$> getCurrentBranch)
	)
  where
	go (LinkAdjustment UnlockAdjustment) = True
	go (LinkAdjustment LockAdjustment) = False
	go (LinkAdjustment FixAdjustment) = False
	go (LinkAdjustment UnFixAdjustment) = False
	go (PresenceAdjustment _ (Just la)) = go (LinkAdjustment la)
	go (PresenceAdjustment _ Nothing) = False
	go (LockUnlockPresentAdjustment UnlockPresentAdjustment) = contentpresent
	go (LockUnlockPresentAdjustment LockPresentAdjustment) = False

{- Adds a file to the work tree for the key, and stages it in the index.
 - The content of the key may be provided in a temp file, which will be
 - moved into place. If no content is provided, adds an annex link but does
 - not ingest the content.
 -
 - When the content of the key is not accepted into the annex, returns False.
 -}
addAnnexedFile :: AddUnlockedMatcher -> OsPath -> Key -> Maybe OsPath -> Annex Bool
addAnnexedFile matcher file key mtmp = ifM (addUnlocked matcher mi (isJust mtmp))
	( do
		mode <- maybe
			(pure Nothing)
			(\tmp -> liftIO $ catchMaybeIO $ fileMode <$> R.getFileStatus (fromOsPath tmp))
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
		addSymlink file key Nothing
		case mtmp of
			Just tmp -> moveAnnex key tmp
			Nothing -> return True
	)
  where
	mi = case mtmp of
		Just tmp -> MatchingFile $ FileInfo
			{ contentFile = tmp
			, matchFile = file
			, matchKey = Just key
			}
		Nothing -> keyMatchInfoWithoutContent key file
	
	linkunlocked mode = linkFromAnnex key file mode >>= \case
		LinkAnnexFailed -> writepointer mode
		_ -> return ()
	
	writepointer mode = liftIO $ writePointerFile file key mode

{- Use with actions that add an already existing annex symlink or pointer
 - file. The warning avoids a confusing situation where the file got copied
 - from another git-annex repo, probably by accident. -}
addingExistingLink :: OsPath -> Key -> Annex a -> Annex a
addingExistingLink f k a = do
	unlessM (isKnownKey k <||> inAnnex k) $ do
		islink <- isJust <$> isAnnexLink f
		warning $
			QuotedPath f
			<> " is a git-annex "
			<> if islink then "symlink." else "pointer file."
			<> " Its content is not available in this repository."
			<> " (Maybe " <> QuotedPath f <> " was copied from another repository?)"
	a
