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
	withFilesInGit (whenAnnexed $ start modmeta) ps

start :: [ModMeta] -> FilePath -> (Key, Backend) -> CommandStart
start ms file (k, _) = do
	showStart "metadata" file
	next $ perform k ms

perform :: Key -> [ModMeta] -> CommandPerform
perform k [] = next $ cleanup k
perform k ms = do
	oldm <- getCurrentMetaData k
	let m = foldl' unionMetaData newMetaData $ map (modMeta oldm) ms
	addMetaData k m
	next $ cleanup k
	
cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	showLongNote $ unlines $ concatMap showmeta $ fromMetaData $ currentMetaData m
	return True
  where
	showmeta (f, vs) = map (\v -> fromMetaField f ++ "=" ++ fromMetaValue v) $ S.toList vs
