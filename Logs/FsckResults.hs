{- git-annex fsck results log files
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.FsckResults (
	writeFsckResults,
	readFsckResults
) where

import Common.Annex
import Utility.Tmp
import Git.Fsck
import Git.Types

import qualified Data.Set as S

writeFsckResults :: UUID -> FsckResults -> Annex ()
writeFsckResults u fsckresults = do
	logfile <- fromRepo $ gitAnnexFsckResultsLog u
	liftIO $ 
		case fsckresults of
			Nothing -> store S.empty logfile
			Just s
				| S.null s -> nukeFile logfile
				| otherwise -> store s logfile
  where
  	store s logfile = do 
		createDirectoryIfMissing True (parentDir logfile)
		liftIO $ viaTmp writeFile logfile $ serialize s
	serialize = unlines . map show . S.toList

readFsckResults :: UUID -> Annex FsckResults
readFsckResults u = do
	logfile <- fromRepo $ gitAnnexFsckResultsLog u
	liftIO $ catchDefaultIO (Just S.empty) $
		deserialize <$> readFile logfile
  where
	deserialize l = 
		let s = S.fromList $ map Ref $ lines l
		in if S.null s then Nothing else Just s
