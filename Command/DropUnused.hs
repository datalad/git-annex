{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
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

type UnusedMap = M.Map Integer Key

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
	return $ map (start (unused, unusedbad, unusedtmp)) $
		concatMap unusedSpec params

unusedSpec :: String -> [Integer]
unusedSpec spec
	| "-" `isInfixOf` spec = range $ separate (== '-') spec
	| otherwise = catMaybes [readish spec]
	where
		range (a, b) = case (readish a, readish b) of
			(Just x, Just y) -> [x..y]
			_ -> []

start :: (UnusedMap, UnusedMap, UnusedMap) -> Integer -> CommandStart
start (unused, unusedbad, unusedtmp) n = search
	[ (unused, perform)
	, (unusedbad, performOther gitAnnexBadLocation)
	, (unusedtmp, performOther gitAnnexTmpLocation)
	]
	where
		search [] = stop
		search ((m, a):rest) =
			case M.lookup n m of
				Nothing -> search rest
				Just key -> do
					showStart "dropunused" (show n)
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
	ifM (liftIO $ doesFileExist f)
		( M.fromList . catMaybes . map parse . lines
			<$> liftIO (readFile f)
		, return M.empty
		)
	where
		parse line =
			case (readish tag, readKey rest) of
				(Just num, Just key) -> Just (num, key)
				_ -> Nothing
			where
				(tag, rest) = separate (== ' ') line
