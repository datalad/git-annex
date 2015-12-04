{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Clean where

import Common.Annex
import Command
import Annex.Content
import Annex.MetaData
import Annex.FileMatcher
import Types.KeySource
import Types.Key
import Backend

import qualified Data.ByteString.Lazy as B

cmd :: Command
cmd = dontCheck repoExists $
	command "clean" SectionPlumbing 
		"git clean filter"
		paramFile (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [file] = do
	ifM (shouldAnnex file)
		( do
			k <- ingest file
			liftIO $ putStrLn (key2file k)
		, liftIO $ B.hGetContents stdin >>= B.hPut stdout -- cat file
		)
	stop
start [] = error "clean filter run without filename; upgrade git"
start _ = error "clean filter passed multiple filenames"

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
	unlessM (linkAnnex k file) $
		error "Problem adding file to the annex"
	genMetaData k file
		=<< liftIO (getFileStatus file)
	return k
