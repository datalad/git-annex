{- git-annex command
 -
 - Copyright 2015-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
import qualified Git.Ref
import Backend

import qualified Data.ByteString.Lazy as B

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
	b <- liftIO $ B.hGetContents stdin
	case parseLinkOrPointer b of
		Nothing -> noop
		Just k -> do
			topfile <- inRepo (toTopFilePath file)
			Database.Keys.addAssociatedFile k topfile
			void $ smudgeLog k topfile
	liftIO $ B.putStr b
	stop

-- Clean filter is fed file content on stdin, decides if a file
-- should be stored in the annex, and outputs a pointer to its
-- injested content if so. Otherwise, the original content.
clean :: FilePath -> CommandStart
clean file = do
	b <- liftIO $ B.hGetContents stdin
	case parseLinkOrPointer b of
		Just k -> do
			getMoveRaceRecovery k file
			liftIO $ B.hPut stdout b
		Nothing -> go b =<< catKeyFile file
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
				then B.length b `seq` return ()
				else liftIO $ hClose stdin
			-- Look up the backend that was used for this file
			-- before, so that when git re-cleans a file its
			-- backend does not change.
			let oldbackend = maybe Nothing (maybeLookupBackendVariety . keyVariety) oldkey
			-- Can't restage associated files because git add
			-- runs this and has the index locked.
			let norestage = Restage False
			liftIO . emitPointer
				=<< postingest
				=<< (\ld -> ingest' oldbackend ld Nothing norestage)
				=<< lockDown cfg file
		, liftIO $ B.hPut stdout b
		)

	postingest (Just k, _) = do
		logStatus k InfoPresent
		return k
	postingest _ = error "could not add file to the annex"

	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmp = False
		}

-- New files are annexed as configured by annex.largefiles, with a default
-- of annexing them.
-- 
-- If annex.largefiles is not configured for a file, and a file with its
-- name is already in the index, preserve its annexed/not annexed state.
-- This prevents accidental conversions when annex.largefiles is being
-- set/unset on the fly rather than being set in gitattributes or .git/config.
shouldAnnex :: FilePath -> Maybe Key -> Annex Bool
shouldAnnex file moldkey = do
	matcher <- largeFilesMatcher
	checkFileMatcher' matcher file whenempty
  where
	whenempty = case moldkey of
		Just _ -> return True
		Nothing -> isNothing <$> catObjectMetaData (Git.Ref.fileRef file)

emitPointer :: Key -> IO ()
emitPointer = putStr . formatPointer

-- Recover from a previous race between eg git mv and git-annex get.
-- That could result in the file remaining a pointer file, while
-- its content is present in the annex. Populate the pointer file.
-- 
-- This also handles the case where a copy of a pointer file is made,
-- then git-annex gets the content, and later git add is run on
-- the pointer copy. It will then be populated with the content.
getMoveRaceRecovery :: Key -> FilePath -> Annex ()
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
	updateSmudged (Restage True)
	stop

updateSmudged :: Restage -> Annex ()
updateSmudged restage = streamSmudged $ \k topf -> do
	f <- fromRepo $ fromTopFilePath topf
	whenM (inAnnex k) $ do
		obj <- calcRepo (gitAnnexLocation k)
		unlessM (isJust <$> populatePointerFile restage k obj f) $
			liftIO (isPointerFile f) >>= \case
				Just k' | k' == k -> toplevelWarning False $
					"unable to populate worktree file " ++ f
				_ -> noop
