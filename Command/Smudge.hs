{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Smudge where

import Command
import qualified Annex
import Annex.Content
import Annex.Link
import Annex.FileMatcher
import Annex.Ingest
import Annex.CatFile
import Logs.Location
import qualified Database.Keys
import Git.FilePath
import Backend

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
			Database.Keys.addAssociatedFile k =<< inRepo (toTopFilePath file)
			-- A previous unlocked checkout of the file may have
			-- led to the annex object getting modified;
			-- don't provide such modified content as it
			-- will be confusing. inAnnex will detect such
			-- modifications.
			ifM (inAnnex k)
				( do
					content <- calcRepo (gitAnnexLocation k)
					whenM (annexThin <$> Annex.getGitConfig) $
						warning $ "Not able to honor annex.thin when git is checking out " ++ file ++ " (run git annex fix to re-thin files)"
					liftIO $ B.putStr . fromMaybe b
						=<< catchMaybeIO (B.readFile content)
				, liftIO $ B.putStr b
				)
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
				-- Even though we ingest the actual file,
				-- and not stdin, we need to consume all
				-- stdin, or git will get annoyed.
				B.length b `seq` return ()
				-- Look up the backend that was used
				-- for this file before, so that when
				-- git re-cleans a file its backend does
				-- not change.
				currbackend <- maybe Nothing (maybeLookupBackendVariety . keyVariety)
					<$> catKeyFile file
				liftIO . emitPointer
					=<< go
					=<< (\ld -> ingest' currbackend ld Nothing)
					=<< lockDown cfg file
			, liftIO $ B.hPut stdout b
			)
	stop
  where
	go (Just k, _) = do
		logStatus k InfoPresent
		return k
	go _ = error "could not add file to the annex"
	cfg = LockDownConfig
		{ lockingFile = False
		, hardlinkFileTmp = False
		}

shouldAnnex :: FilePath -> Annex Bool
shouldAnnex file = do
	matcher <- largeFilesMatcher
	checkFileMatcher matcher file

emitPointer :: Key -> IO ()
emitPointer = putStr . formatPointer
