{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Whereis where

import Control.Monad

import LocationLog
import Command
import Messages
import Remote
import Types
import Trust

command :: [Command]
command = [repoCommand "whereis" paramPaths seek
	"lists repositories that have file content"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: FilePath -> CommandStart
start file = isAnnexed file $ \(key, _) -> do
	showStart "whereis" file
	next $ perform key

perform :: Key -> CommandPerform
perform key = do
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< keyLocations key
	let num = length safelocations
	showNote $ show num ++ " " ++ copiesplural num
	pp <- prettyPrintUUIDs "whereis" safelocations
	unless (null safelocations) $
		showLongNote pp
	pp' <- prettyPrintUUIDs "untrusted" untrustedlocations
	unless (null untrustedlocations) $
		showLongNote $ untrustedheader ++ pp'
	if null safelocations then stop else next $ return True
	where
		copiesplural 1 = "copy"
		copiesplural _ = "copies"
		untrustedheader = "The following untrusted locations may also have copies:\n"
