{- git-annex command
 -
 - Copyright 2015-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Smudge where

import Command
import Annex.Content
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Annex.CatFile
import Annex.WorkTree
import Logs.Smudge
import Logs.Location
import qualified Database.Keys
import qualified Git.BuildVersion
import Git.FilePath
import Git.Types
import Git.HashObject
import qualified Git
import qualified Git.Ref
import qualified Annex
import Backend
import Utility.Metered
import Annex.InodeSentinal
import Utility.InodeCache
import Config.GitConfig
import qualified Types.Backend
import qualified Annex.BranchState

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = noCommit $ noMessages $
	command "smudge" SectionPlumbing 
		"git smudge filter"
		paramFile (seek <$$> optParser)

data SmudgeOptions = UpdateOption | SmudgeOptions
	{ smudgeFile :: FilePath
	, cleanOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser SmudgeOptions
optParser desc = smudgeoptions <|> updateoption
  where
	smudgeoptions = SmudgeOptions
		<$> argument str ( metavar desc )
		<*> switch ( long "clean" <> help "clean filter" )
	updateoption = flag' UpdateOption
		( long "update" <> help "populate annexed worktree files" )

seek :: SmudgeOptions -> CommandSeek
seek (SmudgeOptions f False) = commandAction (smudge f)
seek (SmudgeOptions f True) = commandAction (clean (toRawFilePath f))
seek UpdateOption = commandAction update

-- Smudge filter is fed git file content, and if it's a pointer to an
-- available annex object, git expects it to output its content.
--
-- However, this does not do that. It outputs the pointer, and records
-- the filename in the smudge log. Git hooks run after commands like checkout
-- then run git annex smudge --update which populates the work tree files
-- with annex content. This is done for several reasons:
--
-- * To support annex.thin
-- * Because git currently buffers the whole object received from the
--   smudge filter in memory, which is a problem with large files.
smudge :: FilePath -> CommandStart
smudge file = do
	b <- liftIO $ L.hGetContents stdin
	smudge' file b
	liftIO $ L.putStr b
	stop

-- Handles everything except the IO of the file content.
smudge' :: FilePath -> L.ByteString -> Annex ()
smudge' file b = case parseLinkTargetOrPointerLazy b of
	Nothing -> noop
	Just k -> do
		topfile <- inRepo (toTopFilePath (toRawFilePath file))
		Database.Keys.addAssociatedFile k topfile
		void $ smudgeLog k topfile

-- Clean filter is fed file content on stdin, decides if a file
-- should be stored in the annex, and outputs a pointer to its
-- injested content if so. Otherwise, the original content.
clean :: RawFilePath -> CommandStart
clean file = do
	Annex.BranchState.disableUpdate -- optimisation
	b <- liftIO $ L.hGetContents stdin
	let passthrough = liftIO $ L.hPut stdout b
	-- Before git 2.5, failing to consume all stdin here would
	-- cause a SIGPIPE and crash it.
	-- Newer git catches the signal and stops sending, which is
	-- much faster. (Also, git seems to forget to free memory
	-- when sending the file, so the less we let it send, the
	-- less memory it will waste.)
	let discardreststdin = if Git.BuildVersion.older "2.5"
		then L.length b `seq` return ()
		else liftIO $ hClose stdin
	let emitpointer = liftIO . S.hPut stdout . formatPointer
	clean' file (parseLinkTargetOrPointerLazy' b)
		passthrough
		discardreststdin
		emitpointer
	stop
  where

-- Handles everything except the IO of the file content.
clean'
	:: RawFilePath
	-> Either InvalidAppendedPointerFile (Maybe Key)
	-- ^ If the content provided by git is an annex pointer,
	-- this is the key it points to.
	-> Annex ()
	-- ^ passthrough: Feed the content provided by git back out to git.
	-> Annex ()
	-- ^ discardreststdin: Called when passthrough will not be called,
	-- this has to take care of reading the content provided by git, or
	-- otherwise dealing with it.
	-> (Key -> Annex ())
	-- ^ emitpointer: Emit a pointer file for the key.
	-> Annex ()
clean' file mk passthrough discardreststdin emitpointer =
	ifM (fileOutsideRepo file)
		( passthrough
		, inSmudgeCleanFilter go
		)
  where

	go = case mk of
		Right (Just k) -> do
			addingExistingLink file k $ do
				getMoveRaceRecovery k file
				passthrough
		Right Nothing -> notpointer
		Left InvalidAppendedPointerFile -> do
			toplevelWarning False $
				"The file " <> QuotedPath file <> " looks like git-annex pointer file that has had other content appended to it"
			notpointer

	notpointer = inRepo (Git.Ref.fileRef file) >>= \case
		Just fileref -> do
			indexmeta <- catObjectMetaData fileref
			oldkey <- case indexmeta of
				Just (_, sz, _) -> catKey' fileref sz
				Nothing -> return Nothing
			notpointer' indexmeta oldkey
		Nothing -> passthrough
	
	notpointer' indexmeta oldkey = ifM (shouldAnnex file indexmeta oldkey)
		( do
			discardreststdin

			-- Optimization for the case when the file is already
			-- annexed and is unmodified.
			case oldkey of
				Nothing -> doingest Nothing
				Just ko -> ifM (isUnmodifiedCheap ko file)
					( emitpointer ko
					, updateingest ko
					)
		, passthrough
		)
	
	-- Use the same backend that was used before, when possible.
	-- If the old key's backend does not support generating keys,
	-- use the default backend.
	updateingest oldkey =
		maybeLookupBackendVariety (fromKey keyVariety oldkey) >>= \case
			Nothing -> doingest Nothing
			Just oldbackend -> case Types.Backend.genKey oldbackend of
				Just _ -> doingest (Just oldbackend)
				Nothing -> doingest Nothing
	
	doingest preferredbackend = do
		-- Can't restage associated files because git add
		-- runs this and has the index locked.
		let norestage = Restage False
		emitpointer
			=<< postingest
			=<< (\ld -> ingest' preferredbackend nullMeterUpdate ld Nothing norestage)
			=<< lockDown cfg (fromRawFilePath file)

	postingest (Just k, _) = do
		logStatus NoLiveUpdate k InfoPresent
		return k
	postingest _ = giveup "could not add file to the annex"

	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmpDir = Nothing
		, checkWritePerms = True
		}

-- git diff can run the clean filter on files outside the
-- repository; can't annex those
fileOutsideRepo :: RawFilePath -> Annex Bool
fileOutsideRepo file = do
        repopath <- liftIO . absPath =<< fromRepo Git.repoPath
	filepath <- liftIO $ absPath file
	return $ not $ dirContains repopath filepath

-- Avoid a potential deadlock.
inSmudgeCleanFilter :: Annex a -> Annex a
inSmudgeCleanFilter = bracket setup cleanup . const
  where
	setup = Annex.changeState $ \s -> s
		{ Annex.insmudgecleanfilter = True }
	cleanup () = Annex.changeState $ \s -> s
		{ Annex.insmudgecleanfilter = False }

-- If annex.largefiles is configured (and not disabled by annex.gitaddtoannex
-- being set to false), matching files are added to the annex and the rest to
-- git.
--
-- When annex.largefiles is not configured, files are normally not
-- added to the annex, so will be added to git. However, if the file
-- is annexed in the index, keep it annexed. This prevents accidental
-- conversions when previously annexed files get modified and added.
--
-- In either case, if the file's inode is the same as one that was used
-- for annexed content before, annex it. And if the file is not annexed
-- in the index, and has the same content, leave it in git.
-- This handles cases such as renaming a file followed by git add,
-- which the user naturally expects to behave the same as git mv.
shouldAnnex :: RawFilePath -> Maybe (Sha, FileSize, ObjectType) -> Maybe Key -> Annex Bool
shouldAnnex file indexmeta moldkey = do
	ifM (annexGitAddToAnnex <$> Annex.getGitConfig)
		( checkunchanged $ checkmatcher checkwasannexed
		, checkunchanged checkwasannexed
		)
  where
	checkmatcher d
		| dotfile file = ifM (getGitConfigVal annexDotFiles)
			( go
			, d
			)
		| otherwise = go
	  where
		go = do
			matcher <- largeFilesMatcher
			checkFileMatcher' NoLiveUpdate matcher file d
	
	checkwasannexed = pure $ isJust moldkey

	isknownannexedinode = withTSDelta (liftIO . genInodeCache file) >>= \case
		Nothing -> pure False
		Just ic -> Database.Keys.isInodeKnown ic =<< sentinalStatus

	-- If the inode matches one known used for annexed content,
	-- keep the file annexed. This handles a case where the file
	-- has been annexed before, and the git is running the clean filter
	-- again on it for whatever reason.
	checkunchanged cont = ifM isknownannexedinode
		( return True
		, checkunchangedgitfile cont
		)

	-- This checks for a case where the file had been added to git
	-- previously, not to the annex before, and its content is not
	-- changed, but git is running the clean filter again on it
	-- (eg because its mtime or inode changed, or just because git feels
	-- like it). Such a file should not be added to the annex, even if
	-- annex.largefiles now matches it, because the content is not
	-- changed.
	checkunchangedgitfile cont = case (moldkey, indexmeta) of
		(Nothing, Just (sha, sz, _)) -> liftIO (catchMaybeIO (getFileSize file)) >>= \case
			Just sz' | sz' == sz -> do
				-- The size is the same, so the file
				-- is not much larger than what was stored
				-- in git before, so it won't be out of
				-- line to hash it. However, the content
				-- is prevented from being stored in git
				-- when hashing.
				h <- inRepo $ hashObjectStart False
				sha' <- liftIO $ hashFile h file
				liftIO $ hashObjectStop h
				if sha' == sha
					then return False
					else cont
			_ -> cont
		_ -> cont

-- Recover from a previous race between eg git mv and git-annex get.
-- That could result in the file remaining a pointer file, while
-- its content is present in the annex. Populate the pointer file.
-- 
-- This also handles the case where a copy of a pointer file is made,
-- then git-annex gets the content, and later git add is run on
-- the pointer copy. It will then be populated with the content.
getMoveRaceRecovery :: Key -> RawFilePath -> Annex ()
getMoveRaceRecovery k file = void $ tryNonAsync $
	whenM (inAnnex k) $ do
		obj <- calcRepo (gitAnnexLocation k)
		-- Cannot restage because git add is running and has
		-- the index locked.
		populatePointerFile (Restage False) k obj file >>= \case
			Nothing -> return ()
			Just ic -> Database.Keys.addInodeCaches k [ic]

update :: CommandStart
update = do
	-- This gets run after a git checkout or merge, so it's a good
	-- point to refresh the keys database for changes to annexed files.
	-- Doing it explicitly here avoids a later pause in the middle of
	-- some other action.
	scanAnnexedFiles
	updateSmudged (Restage True)
	stop

updateSmudged :: Restage -> Annex ()
updateSmudged restage = streamSmudged $ \k topf -> do
	f <- fromRepo (fromTopFilePath topf)
	whenM (inAnnex k) $ do
		obj <- calcRepo (gitAnnexLocation k)
		objic <- withTSDelta (liftIO . genInodeCache obj)
		populatePointerFile restage k obj f >>= \case
			Just ic -> do
				cs <- Database.Keys.getInodeCaches k
				if null cs
					then Database.Keys.addInodeCaches k (catMaybes [Just ic, objic])
					else Database.Keys.addInodeCaches k [ic]
			Nothing -> liftIO (isPointerFile f) >>= \case
				Just k' | k' == k -> toplevelWarning False $
					"unable to populate worktree file " <> QuotedPath f
				_ -> noop
