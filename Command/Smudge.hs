{- git-annex command
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Smudge where

import Command
import Annex.Content
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Annex.CatFile
import Logs.Smudge
import Logs.Location
import qualified Database.Keys
import qualified Git.BuildVersion
import Git.FilePath
import qualified Git
import qualified Annex
import Backend
import Utility.Metered
import Annex.InodeSentinal
import Utility.InodeCache

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
seek (SmudgeOptions f True) = commandAction (clean f)
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
	case parseLinkTargetOrPointerLazy b of
		Nothing -> noop
		Just k -> do
			topfile <- inRepo (toTopFilePath (toRawFilePath file))
			Database.Keys.addAssociatedFile k topfile
			void $ smudgeLog k topfile
	liftIO $ L.putStr b
	stop

-- Clean filter is fed file content on stdin, decides if a file
-- should be stored in the annex, and outputs a pointer to its
-- injested content if so. Otherwise, the original content.
clean :: FilePath -> CommandStart
clean file = do
	b <- liftIO $ L.hGetContents stdin
	ifM fileoutsiderepo
		( liftIO $ L.hPut stdout b
		, case parseLinkTargetOrPointerLazy b of
			Just k -> do
				getMoveRaceRecovery k (toRawFilePath file)
				liftIO $ L.hPut stdout b
			Nothing -> go b =<< catKeyFile (toRawFilePath file)
		)
	stop
  where
	go b oldkey = ifM (shouldAnnex file oldkey)
		( do
			-- Before git 2.5, failing to consume all stdin here
			-- would cause a SIGPIPE and crash it.
			-- Newer git catches the signal and stops sending,
			-- which is much faster. (Also, git seems to forget
			-- to free memory when sending the file, so the
			-- less we let it send, the less memory it will waste.)
			if Git.BuildVersion.older "2.5"
				then L.length b `seq` return ()
				else liftIO $ hClose stdin

			-- Optimization for the case when the file is already
			-- annexed and is unmodified.
			case oldkey of
				Nothing -> doingest oldkey
				Just ko -> ifM (isUnmodifiedCheap ko file)
					( liftIO $ emitPointer ko
					, doingest oldkey
					)
		, liftIO $ L.hPut stdout b
		)
	
	doingest oldkey = do
		-- Look up the backend that was used for this file
		-- before, so that when git re-cleans a file its
		-- backend does not change.
		let oldbackend = maybe Nothing (maybeLookupBackendVariety . fromKey keyVariety) oldkey
		-- Can't restage associated files because git add
		-- runs this and has the index locked.
		let norestage = Restage False
		liftIO . emitPointer
			=<< postingest
			=<< (\ld -> ingest' oldbackend nullMeterUpdate ld Nothing norestage)
			=<< lockDown cfg file

	postingest (Just k, _) = do
		logStatus k InfoPresent
		return k
	postingest _ = error "could not add file to the annex"

	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmpDir = Nothing
		}

	-- git diff can run the clean filter on files outside the
	-- repository; can't annex those
	fileoutsiderepo = do
	        repopath <- liftIO . absPath . fromRawFilePath
			=<< fromRepo Git.repoPath
		filepath <- liftIO $ absPath file
		return $ not $ dirContains repopath filepath

-- If annex.largefiles is configured, matching files are added to the
-- annex. But annex.gitaddtoannex can be set to false to disable that.
--
-- When annex.largefiles is not configured, files are normally not
-- added to the annex, so will be added to git. But some heuristics
-- are used to avoid bad behavior:
--
-- If the index already contains the file, preserve its annexed/not annexed
-- state. This prevents accidental conversions.
--
-- Otherwise, when the file's inode is the same as one that was used for
-- annexed content before, annex it. This handles cases such as renaming an
-- unlocked annexed file followed by git add, which the user naturally
-- expects to behave the same as git mv.
shouldAnnex :: FilePath -> Maybe Key -> Annex Bool
shouldAnnex file moldkey = ifM (annexGitAddToAnnex <$> Annex.getGitConfig)
	( checkmatcher checkheuristics
	, checkheuristics
	)
  where
	checkmatcher d = do
		matcher <- largeFilesMatcher
		checkFileMatcher' matcher file d
	
	checkheuristics = case moldkey of
		Just _ -> return True
		Nothing -> checkknowninode

	checkknowninode = withTSDelta (liftIO . genInodeCache file) >>= \case
		Nothing -> pure False
		Just ic -> Database.Keys.isInodeKnown ic =<< sentinalStatus

emitPointer :: Key -> IO ()
emitPointer = S.putStr . formatPointer

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
		obj <- toRawFilePath <$> calcRepo (gitAnnexLocation k)
		-- Cannot restage because git add is running and has
		-- the index locked.
		populatePointerFile (Restage False) k obj file >>= \case
			Nothing -> return ()
			Just ic -> Database.Keys.addInodeCaches k [ic]

update :: CommandStart
update = do
	updateSmudged (Restage True)
	stop

updateSmudged :: Restage -> Annex ()
updateSmudged restage = streamSmudged $ \k topf -> do
	f <- fromRepo (fromTopFilePath topf)
	whenM (inAnnex k) $ do
		obj <- toRawFilePath <$> calcRepo (gitAnnexLocation k)
		unlessM (isJust <$> populatePointerFile restage k obj f) $
			liftIO (isPointerFile f) >>= \case
				Just k' | k' == k -> toplevelWarning False $
					"unable to populate worktree file " ++ fromRawFilePath f
				_ -> noop
