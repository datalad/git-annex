{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Control.Monad.State (liftIO)
import qualified Data.Map as M
import System.Directory
import Data.Maybe

import Command
import Types
import Messages
import Locations
import qualified Annex
import qualified Command.Drop
import qualified Command.Move
import qualified Remote
import qualified Git
import Types.Key
import Utility

type UnusedMap = M.Map String Key

command :: [Command]
command = [repoCommand "dropunused" (paramRepeating paramNumber) seek
	"drop unused file content"]

seek :: [CommandSeek]
seek = [withUnusedMaps]

{- Read unused logs once, and pass the maps to each start action. -}
withUnusedMaps :: CommandSeek
withUnusedMaps params = do
	unused <- readUnusedLog ""
	unusedbad <- readUnusedLog "bad"
	unusedtmp <- readUnusedLog "tmp"
	return $ map (start (unused, unusedbad, unusedtmp)) params

start :: (UnusedMap, UnusedMap, UnusedMap) -> CommandStartString
start (unused, unusedbad, unusedtmp) s = notBareRepo $ search
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
perform key = maybe droplocal dropremote =<< Annex.getState Annex.fromremote
	where
		dropremote name = do
			r <- Remote.byName name
			showAction $ "from " ++ Remote.name r
			next $ Command.Move.fromCleanup r True key
		droplocal = Command.Drop.perform key (Just 0) -- force drop

performOther :: (Git.Repo -> Key -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	g <- Annex.gitRepo
	let f = filespec g key
	liftIO $ whenM (doesFileExist f) $ removeFile f
	next $ return True

readUnusedLog :: FilePath -> Annex UnusedMap
readUnusedLog prefix = do
	g <- Annex.gitRepo
	let f = gitAnnexUnusedLog prefix g
	e <- liftIO $ doesFileExist f
	if e
		then return . M.fromList . map parse . lines
			=<< liftIO (readFile f)
		else return M.empty
	where
		parse line = (head ws, fromJust $ readKey $ unwords $ tail ws)
			where
				ws = words line
