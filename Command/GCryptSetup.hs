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
	g <- gitRepo
	u <- getUUID
	gu <- Remote.GCrypt.getGCryptUUID True g
	if u == NoUUID && gu == Nothing
		then if Git.repoIsLocalBare g
			then do
				void $ Remote.GCrypt.setupRepo gcryptid g
				return True
			else error "cannot use gcrypt in a non-bare repository"
		else error "gcryptsetup permission denied"
