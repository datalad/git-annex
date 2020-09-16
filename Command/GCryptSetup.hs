{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.GCryptSetup where

import Command
import Annex.UUID
import qualified Remote.GCrypt
import qualified Git

cmd :: Command
cmd = dontCheck repoExists $ noCommit $
	command "gcryptsetup" SectionPlumbing 
		"sets up gcrypt repository"
		paramValue (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withStrings (commandAction . start)

start :: String -> CommandStart
start gcryptid = starting "gcryptsetup" (ActionItemOther Nothing) (SeekInput [gcryptid]) $ do
	u <- getUUID
	when (u /= NoUUID) $
		giveup "gcryptsetup refusing to run; this repository already has a git-annex uuid!"
	
	g <- gitRepo
	gu <- Remote.GCrypt.getGCryptUUID True g
	let newgu = genUUIDInNameSpace gCryptNameSpace gcryptid
	if isNothing gu || gu == Just newgu
		then if Git.repoIsLocalBare g
			then do
				void $ Remote.GCrypt.setupRepo gcryptid g
				next $ return True
			else giveup "cannot use gcrypt in a non-bare repository"
		else giveup "gcryptsetup uuid mismatch"
