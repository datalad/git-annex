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
import Annex.MetaData
import Logs.MetaData
import Types.MetaData

import qualified Data.Set as S
import Data.Time.Clock.POSIX

def :: [Command]
def = [withOptions [setOption, tagOption, untagOption] $
	command "metadata" paramPaths seek
	SectionMetaData "sets metadata of a file"]

storeModMeta :: ModMeta -> Annex ()
storeModMeta modmeta = Annex.changeState $
	\s -> s { Annex.modmeta = modmeta:Annex.modmeta s }

setOption :: Option
setOption = Option ['s'] ["set"] (ReqArg mkmod "FIELD[+-]=VALUE") "set metadata"
  where
	mkmod = either error storeModMeta . parseModMeta

tagOption :: Option
tagOption = Option ['t'] ["tag"] (ReqArg mkmod "TAG") "set a tag"
  where
	mkmod = storeModMeta . AddMeta tagMetaField . toMetaValue

untagOption :: Option
untagOption = Option ['u'] ["untag"] (ReqArg mkmod "TAG") "remove a tag"
  where
	mkmod = storeModMeta . AddMeta tagMetaField . mkMetaValue (CurrentlySet False)

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
	let m = foldl' unionMetaData emptyMetaData $ map (modMeta oldm) ms
	addMetaData' k m now
	next $ cleanup k
	
cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	showLongNote $ unlines $ concatMap showmeta $ fromMetaData $ currentMetaData m
	return True
  where
	showmeta (f, vs) = map (\v -> fromMetaField f ++ "=" ++ fromMetaValue v) $ S.toList vs
