{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import LocationLog
import Command
import Messages
import Remote
import Types

command :: [Command]
command = [repoCommand "whereis" (paramOptional $ paramRepeating paramPath) seek
	"lists repositories that have file content"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	showStart "whereis" file
	next $ perform key

perform :: Key -> CommandPerform
perform key = do
	uuids <- keyLocations key
	let num = length uuids
	showNote $ show num ++ " " ++ copiesplural num
	if null uuids
		then stop
		else do
			pp <- prettyPrintUUIDs uuids
			showLongNote pp
			showOutput
			next $ return True
	where
		copiesplural 1 = "copy"
		copiesplural _ = "copies"
