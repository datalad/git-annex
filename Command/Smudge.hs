{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Smudge where

import Common.Annex
import Command
import Types.Key
import Annex.Content
import Annex.MetaData
import Annex.FileMatcher
import Types.KeySource
import Backend
import Logs.Location

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

-- Smudge filter is fed git file content, and if it's a pointer, should
-- emit the annex object content.
smudge :: FilePath -> CommandStart
smudge _file = do
	liftIO $ fileEncoding stdin
	s <- liftIO $ hGetContents stdin
	case parsePointer s of
		Nothing -> liftIO $ putStr s
		Just k -> do
			content <- calcRepo (gitAnnexLocation k)
			liftIO $ maybe
				(putStr s)
				(B.hPut stdout)
				=<< catchMaybeIO (B.readFile content)
	stop

-- Clean filter decides if a file should be stored in the annex, and
-- outputs a pointer to its injested content.
clean :: FilePath -> CommandStart
clean file = do
	ifM (shouldAnnex file)
		( do
			k <- ingest file
			liftIO $ emitPointer k
		, liftIO $ B.hGetContents stdin >>= B.hPut stdout -- cat file
		)
	stop

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
	-- Hard link (or copy) file content to annex
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
emitPointer = putStrLn . key2file

parsePointer :: String -> Maybe Key
parsePointer s
	| length s' >= maxsz = Nothing -- too long to be a key pointer
	| otherwise = headMaybe (lines s') >>= file2key
  where
	s' = take maxsz s
	maxsz = 81920
