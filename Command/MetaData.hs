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

import qualified Data.Set as S
import Data.Time.Clock.POSIX

def :: [Command]
def = [withOptions metaDataOptions $
	command "metadata" paramPaths seek
	SectionMetaData "sets metadata of a file"]

metaDataOptions :: [Option]
metaDataOptions =
	[ setOption
	, tagOption
	, untagOption
	, getOption
	, jsonOption
	] ++ keyOptions

storeModMeta :: ModMeta -> Annex ()
storeModMeta modmeta = Annex.changeState $
	\s -> s { Annex.modmeta = modmeta:Annex.modmeta s }

setOption :: Option
setOption = Option ['s'] ["set"] (ReqArg mkmod "FIELD[+-]=VALUE") "set metadata"
  where
	mkmod = either error storeModMeta . parseModMeta

getOption :: Option
getOption = fieldOption ['g'] "get" paramField "get single metadata field"

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
	getfield <- getOptionField getOption $ \ms ->
		return $ either error id . mkMetaField <$> ms
	now <- liftIO getPOSIXTime
	withKeyOptions
		(startKeys now getfield modmeta)
		(withFilesInGit (whenAnnexed $ start now getfield modmeta))
		ps

start :: POSIXTime -> Maybe MetaField -> [ModMeta] -> FilePath -> Key -> CommandStart
start now f ms file = start' (Just file) now f ms

startKeys :: POSIXTime -> Maybe MetaField -> [ModMeta] -> Key -> CommandStart
startKeys = start' Nothing

start' :: AssociatedFile -> POSIXTime -> Maybe MetaField -> [ModMeta] -> Key -> CommandStart
start' afile now Nothing ms k = do
	showStart' "metadata" k afile
	next $ perform now ms k
start' _ _ (Just f) _ k = do
	l <- S.toList . currentMetaDataValues f <$> getCurrentMetaData k
	liftIO $ forM_ l $
		putStrLn . fromMetaValue
	stop

perform :: POSIXTime -> [ModMeta] -> Key -> CommandPerform
perform _ [] k = next $ cleanup k
perform now ms k = do
	oldm <- getCurrentMetaData k
	let m = combineMetaData $ map (modMeta oldm) ms
	addMetaData' k m now
	next $ cleanup k
	
cleanup :: Key -> CommandCleanup
cleanup k = do
	l <- map unwrapmeta . fromMetaData <$> getCurrentMetaData k
	maybeShowJSON l
	showLongNote $ unlines $ concatMap showmeta l
	return True
  where
	unwrapmeta (f, v) = (fromMetaField f, map fromMetaValue (S.toList v))
	showmeta (f, vs) = map ((f ++ "=") ++) vs
