{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.GCryptSetup where

import Common.Annex
import Command
import Annex.UUID
import qualified Remote.GCrypt
import qualified Git

def :: [Command]
def = [dontCheck repoExists $ noCommit $
	command "gcryptsetup" paramValue seek
		SectionPlumbing "sets up gcrypt repository"]

seek :: [CommandSeek]
seek = [withStrings start]

start :: String -> CommandStart
start gcryptid = next $ next $ do
	u <- getUUID
	when (u /= NoUUID) $
		error "gcryptsetup refusing to run; this repository already has a git-annex uuid!"
	
	g <- gitRepo
	gu <- Remote.GCrypt.getGCryptUUID True g
	let newgu = genUUIDInNameSpace gCryptNameSpace gcryptid
	if gu == Nothing || gu == Just newgu
		then if Git.repoIsLocalBare g
			then do
				void $ Remote.GCrypt.setupRepo gcryptid g
				return True
			else error "cannot use gcrypt in a non-bare repository"
		else error "gcryptsetup uuid mismatch"
