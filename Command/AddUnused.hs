{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUnused where

import Common.Annex
import Logs.Location
import Command
import qualified Command.Add
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)
import Types.Key

cmd :: Command
cmd = notDirect $ 
	command "addunused" SectionMaintenance 
		"add back unused files"
		(paramRepeating paramNumRange) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withUnusedMaps start

start :: UnusedMaps -> Int -> CommandStart
start = startUnused "addunused" perform
	(performOther "bad")
	(performOther "tmp")

perform :: Key -> CommandPerform
perform key = next $ do
	logStatus key InfoPresent
	Command.Add.addLink file key Nothing
	return True
  where
	file = "unused." ++ key2file key

{- The content is not in the annex, but in another directory, and
 - it seems better to error out, rather than moving bad/tmp content into
 - the annex. -}
performOther :: String -> Key -> CommandPerform
performOther other _ = error $ "cannot addunused " ++ other ++ "content"
