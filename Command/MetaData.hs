{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.MetaData where

import Common.Annex
import qualified Annex
import Command
import Logs.MetaData
import Types.MetaData

import qualified Data.Set as S
import Data.Time.Clock.POSIX

def :: [Command]
def = [withOptions [setOption] $ command "metadata" paramPaths seek
	SectionUtility "sets metadata of a file"]

setOption :: Option
setOption = Option ['s'] ["set"] (ReqArg mkmod "field[+-]=value") "set metadata"
  where
	mkmod p = case parseModMeta p of
		Left e -> error e
		Right modmeta -> Annex.changeState $
			\s -> s { Annex.modmeta = modmeta:Annex.modmeta s }

seek :: CommandSeek
seek ps = do
	modmeta <- Annex.getState Annex.modmeta
	now <- liftIO getPOSIXTime
	withFilesInGit (whenAnnexed $ start now modmeta) ps

start :: POSIXTime -> [ModMeta] -> FilePath -> (Key, Backend) -> CommandStart
start now ms file (k, _) = do
	showStart "metadata" file
	next $ perform now ms k

perform :: POSIXTime -> [ModMeta] -> Key -> CommandPerform
perform _ [] k = next $ cleanup k
perform now ms k = do
	oldm <- getCurrentMetaData k
	let m = foldl' unionMetaData newMetaData $ map (modMeta oldm) ms
	addMetaData' k m now
	next $ cleanup k
	
cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	showLongNote $ unlines $ concatMap showmeta $ fromMetaData $ currentMetaData m
	return True
  where
	showmeta (f, vs) = map (\v -> fromMetaField f ++ "=" ++ fromMetaValue v) $ S.toList vs
