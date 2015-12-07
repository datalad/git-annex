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
import Annex.CatFile
import Annex.MetaData
import Annex.FileMatcher
import Types.KeySource
import Backend
import Logs.Location
import qualified Database.AssociatedFiles as AssociatedFiles

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
	liftIO $ fileEncoding stdin
	s <- liftIO $ hGetContents stdin
	case parsePointer s of
		Nothing -> liftIO $ putStr s
		Just k -> do
			updateAssociatedFiles k file
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
			updateAssociatedFiles k file
			liftIO $ emitPointer k
		, liftIO cat
		)
	stop

cat :: IO ()
cat = B.hGetContents stdin >>= B.hPut stdout

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

-- Could add a newline and some text explaining this file is a pointer.
-- parsePointer only looks at the first line.
emitPointer :: Key -> IO ()
emitPointer = putStrLn . key2file

updateAssociatedFiles :: Key -> FilePath -> Annex ()
updateAssociatedFiles k f = do
	h <- AssociatedFiles.openDb
	liftIO $ do
		AssociatedFiles.addDb h k f
		AssociatedFiles.closeDb h
