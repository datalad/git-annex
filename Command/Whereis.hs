{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import Common.Annex
import Logs.Location
import Command
import Remote
import Logs.Trust

def :: [Command]
def = [command "whereis" paramPaths seek
	"lists repositories that have file content"]

seek :: [CommandSeek]
seek = [withFilesInGit $ whenAnnexed start]

start :: FilePath -> (Key, Backend) -> CommandStart
start file (key, _) = do
	showStart "whereis" file
	next $ perform key

perform :: Key -> CommandPerform
perform key = do
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< keyLocations key
	let num = length safelocations
	showNote $ show num ++ " " ++ copiesplural num
	pp <- prettyPrintUUIDs "whereis" safelocations
	unless (null safelocations) $ showLongNote pp
	pp' <- prettyPrintUUIDs "untrusted" untrustedlocations
	unless (null untrustedlocations) $ showLongNote $ untrustedheader ++ pp'
	if null safelocations then stop else next $ return True
	where
		copiesplural 1 = "copy"
		copiesplural _ = "copies"
		untrustedheader = "The following untrusted locations may also have copies:\n"
