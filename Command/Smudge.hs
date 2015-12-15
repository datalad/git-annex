{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Smudge where

import Common.Annex
import Command
import Annex.Content
import Annex.Link
import Annex.MetaData
import Annex.FileMatcher
import Annex.InodeSentinal
import Types.KeySource
import Backend
import Logs.Location
import qualified Database.Keys

import qualified Data.ByteString.Lazy as B

cmd :: Command
cmd = noCommit $ noMessages $
	command "smudge" SectionPlumbing 
		"git smudge filter"
		paramFile (seek <$$> optParser)

data SmudgeOptions = SmudgeOptions
	{ smudgeFile :: FilePath
	, cleanOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser SmudgeOptions
optParser desc = SmudgeOptions
	<$> argument str ( metavar desc )
	<*> switch ( long "clean" <> help "clean filter" )

seek :: SmudgeOptions -> CommandSeek
seek o = commandAction $
	(if cleanOption o then clean else smudge) (smudgeFile o)

-- Smudge filter is fed git file content, and if it's a pointer to an
-- available annex object, should output its content.
smudge :: FilePath -> CommandStart
smudge file = do
	b <- liftIO $ B.hGetContents stdin
	case parseLinkOrPointer b of
		Nothing -> liftIO $ B.putStr b
		Just k -> do
			-- A previous unlocked checkout of the file may have
			-- led to the annex object getting modified;
			-- don't provide such modified content as it
			-- will be confusing. inAnnex will detect such
			-- modifications.
			ifM (inAnnex k)
				( do
					content <- calcRepo (gitAnnexLocation k)
					liftIO $ B.putStr . fromMaybe b
						=<< catchMaybeIO (B.readFile content)
				, liftIO $ B.putStr b
				)
			Database.Keys.addAssociatedFile k file
	stop

-- Clean filter is fed file content on stdin, decides if a file
-- should be stored in the annex, and outputs a pointer to its
-- injested content.
clean :: FilePath -> CommandStart
clean file = do
	b <- liftIO $ B.hGetContents stdin
	if isJust (parseLinkOrPointer b)
		then liftIO $ B.hPut stdout b
		else ifM (shouldAnnex file)
			( do
				k <- ingest file
				oldkeys <- filter (/= k)
					<$> Database.Keys.getAssociatedKey file
				mapM_ (cleanOldKey file) oldkeys
				Database.Keys.addAssociatedFile k file
				liftIO $ emitPointer k
			, liftIO $ B.hPut stdout b
			)
	stop

-- If the file being cleaned was hard linked to the old key's annex object,
-- modifying the file will have caused the object to have the wrong content.
-- Clean up from that, making the 
cleanOldKey :: FilePath -> Key -> Annex ()
cleanOldKey modifiedfile key = do
	obj <- calcRepo (gitAnnexLocation key)
	caches <- Database.Keys.getInodeCaches key
	unlessM (sameInodeCache obj caches) $ do
		unlinkAnnex key
		fs <- filter (/= modifiedfile)
			<$> Database.Keys.getAssociatedFiles key
		fs' <- filterM (`sameInodeCache` caches) fs
		case fs' of
			-- If linkAnnex fails, the file with the content
			-- is still present, so no need for any recovery.
			(f:_) -> void $ linkAnnex key f
			_ -> lostcontent
  where
	lostcontent = logStatus key InfoMissing

shouldAnnex :: FilePath -> Annex Bool
shouldAnnex file = do
	matcher <- largeFilesMatcher
	checkFileMatcher matcher file

ingest :: FilePath -> Annex Key
ingest file = do
	backend <- chooseBackend file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = file
		, inodeCache = Nothing
		}
	k <- fst . fromMaybe (error "failed to generate a key")
		<$> genKey source backend
	-- Hard link (or copy) file content to annex object
	-- to prevent it from being lost when git checks out
	-- a branch not containing this file.
	r <- linkAnnex k file
	case r of
		LinkAnnexFailed -> error "Problem adding file to the annex"
		LinkAnnexOk -> logStatus k InfoPresent
		LinkAnnexNoop -> noop
	genMetaData k file
		=<< liftIO (getFileStatus file)
	return k

emitPointer :: Key -> IO ()
emitPointer = putStr . formatPointer
