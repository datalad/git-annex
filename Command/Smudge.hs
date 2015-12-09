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
	b <- liftIO $ B.hGetContents stdin
	case parseLinkOrPointer b of
		Nothing -> liftIO $ B.putStr b
		Just k -> do
			updateAssociatedFiles k file
			content <- calcRepo (gitAnnexLocation k)
			liftIO $ B.hPut stdout . fromMaybe b
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

emitPointer :: Key -> IO ()
emitPointer = putStrLn . formatPointer

updateAssociatedFiles :: Key -> FilePath -> Annex ()
updateAssociatedFiles k f = do
	AssociatedFiles.addDb k f
	AssociatedFiles.flushDb
