{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
seek = withStrings start

start :: String -> CommandStart
start gcryptid = next $ next $ do
	u <- getUUID
	when (u /= NoUUID) $
		error "gcryptsetup refusing to run; this repository already has a git-annex uuid!"
	
	g <- gitRepo
	gu <- Remote.GCrypt.getGCryptUUID True g
	let newgu = genUUIDInNameSpace gCryptNameSpace gcryptid
	if isNothing gu || gu == Just newgu
		then if Git.repoIsLocalBare g
			then do
				void $ Remote.GCrypt.setupRepo gcryptid g
				return True
			else error "cannot use gcrypt in a non-bare repository"
		else error "gcryptsetup uuid mismatch"
