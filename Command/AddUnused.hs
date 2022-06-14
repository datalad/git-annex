{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.AddUnused where

import Logs.Location
import Command
import Annex.Ingest
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)

cmd :: Command
cmd = command "addunused" SectionMaintenance 
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
	addSymlink file key Nothing
	return True
  where
	file = "unused." <> keyFile key

{- The content is not in the annex, but in another directory, and
 - it seems better to error out, rather than moving bad/tmp content into
 - the annex. -}
performOther :: String -> Key -> CommandPerform
performOther other _ = giveup $ "cannot addunused " ++ other ++ "content"
