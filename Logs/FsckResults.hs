{- git-annex fsck results log files
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.FsckResults (
	writeFsckResults,
	readFsckResults,
	clearFsckResults,
) where

import Annex.Common
import Git.Fsck
import Git.Types
import Logs.File

import qualified Data.Set as S

writeFsckResults :: UUID -> FsckResults -> Annex ()
writeFsckResults u fsckresults = do
	logfile <- fromRepo $ gitAnnexFsckResultsLog u
	case fsckresults of
		FsckFailed -> store S.empty False logfile
		FsckFoundMissing s t
			| S.null s -> liftIO $ nukeFile logfile
			| otherwise -> store s t logfile
  where
	store s t logfile = writeLogFile logfile $ serialize s t
	serialize s t =
		let ls = map fromRef (S.toList s)
		in if t
			then unlines ("truncated":ls)
			else unlines ls

readFsckResults :: UUID -> Annex FsckResults
readFsckResults u = do
	logfile <- fromRepo $ gitAnnexFsckResultsLog u
	liftIO $ catchDefaultIO (FsckFoundMissing S.empty False) $
		deserialize . lines <$> readFile logfile
  where
	deserialize ("truncated":ls) = deserialize' ls True
	deserialize ls = deserialize' ls False
	deserialize' ls t =
		let s = S.fromList $ map (Ref . encodeBS') ls
		in if S.null s then FsckFailed else FsckFoundMissing s t

clearFsckResults :: UUID -> Annex ()
clearFsckResults = liftIO . nukeFile <=< fromRepo . gitAnnexFsckResultsLog
	
