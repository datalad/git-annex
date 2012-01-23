{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import qualified Data.Map as M

import Common.Annex
import Command
import qualified Annex
import qualified Command.Drop
import qualified Remote
import qualified Git
import qualified Option
import Types.Key

type UnusedMap = M.Map String Key

def :: [Command]
def = [withOptions [Command.Drop.fromOption] $
	command "dropunused" (paramRepeating paramNumber)
		seek "drop unused file content"]

seek :: [CommandSeek]
seek = [withUnusedMaps]

{- Read unused logs once, and pass the maps to each start action. -}
withUnusedMaps :: CommandSeek
withUnusedMaps params = do
	unused <- readUnusedLog ""
	unusedbad <- readUnusedLog "bad"
	unusedtmp <- readUnusedLog "tmp"
	return $ map (start (unused, unusedbad, unusedtmp)) params

start :: (UnusedMap, UnusedMap, UnusedMap) -> FilePath -> CommandStart
start (unused, unusedbad, unusedtmp) s = search
	[ (unused, perform)
	, (unusedbad, performOther gitAnnexBadLocation)
	, (unusedtmp, performOther gitAnnexTmpLocation)
	]
	where
		search [] = stop
		search ((m, a):rest) =
			case M.lookup s m of
				Nothing -> search rest
				Just key -> do
					showStart "dropunused" s
					next $ a key

perform :: Key -> CommandPerform
perform key = maybe droplocal dropremote =<< Remote.byName =<< from
	where
		dropremote r = do
			showAction $ "from " ++ Remote.name r
			ok <- Remote.removeKey r key
			next $ Command.Drop.cleanupRemote key r ok
		droplocal = Command.Drop.performLocal key (Just 0) -- force drop
		from = Annex.getField $ Option.name Command.Drop.fromOption

performOther :: (Key -> Git.Repo -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	liftIO $ whenM (doesFileExist f) $ removeFile f
	next $ return True

readUnusedLog :: FilePath -> Annex UnusedMap
readUnusedLog prefix = do
	f <- fromRepo $ gitAnnexUnusedLog prefix
	e <- liftIO $ doesFileExist f
	if e
		then M.fromList . map parse . lines <$> liftIO (readFile f)
		else return M.empty
	where
		parse line = (num, fromJust $ readKey rest)
			where
				(num, rest) = separate (== ' ') line
