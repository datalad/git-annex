{- git-annex fsck results log files
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.FsckResults (
	writeFsckResults,
	readFsckResults,
	clearFsckResults,
) where

import Annex.Common
import Utility.Tmp
import Git.Fsck
import Git.Types

import qualified Data.Set as S

writeFsckResults :: UUID -> FsckResults -> Annex ()
writeFsckResults u fsckresults = do
	logfile <- fromRepo $ gitAnnexFsckResultsLog u
	liftIO $ 
		case fsckresults of
			FsckFailed -> store S.empty False logfile
			FsckFoundMissing s t
				| S.null s -> nukeFile logfile
				| otherwise -> store s t logfile
  where
	store s t logfile = do 
		createDirectoryIfMissing True (parentDir logfile)
		liftIO $ viaTmp writeFile logfile $ serialize s t
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
		let s = S.fromList $ map Ref ls
		in if S.null s then FsckFailed else FsckFoundMissing s t

clearFsckResults :: UUID -> Annex ()
clearFsckResults = liftIO . nukeFile <=< fromRepo . gitAnnexFsckResultsLog
	
