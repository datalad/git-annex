{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUnused where

import Common.Annex
import Logs.Unused
import Command
import qualified Command.Add

def :: [Command]
def = [command "addunused" (paramRepeating paramNumRange)
	seek "add back unused files"]

seek :: [CommandSeek]
seek = [withUnusedMaps start]

start :: UnusedMaps -> Int -> CommandStart
start = startUnused "addunused" perform (performOther "bad") (performOther "tmp")

perform :: Key -> CommandPerform
perform key = next $ Command.Add.cleanup file key True
	where
		file = "unused." ++ show key

{- The content is not in the annex, but in another directory, and
 - it seems better to error out, rather than moving bad/tmp content into
 - the annex. -}
performOther :: String -> Key -> CommandPerform
performOther other _ = error $ "cannot addunused " ++ other ++ "content"
