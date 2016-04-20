{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinject where

import Command
import Logs.Location
import Annex.Content

cmd :: Command
cmd = command "reinject" SectionUtility 
	"sets content of annexed file"
	(paramPair "SRC" "DEST") (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [FilePath] -> CommandStart
start (src:dest:[])
	| src == dest = stop
	| otherwise =
		ifAnnexed src
			(error $ "cannot used annexed file as src: " ++ src)
			go
  where
	go = do
		showStart "reinject" dest
		next $ whenAnnexed (perform src) dest
start _ = error "specify a src file and a dest file"

perform :: FilePath -> FilePath -> Key -> CommandPerform
perform src _dest key = ifM move
	( next $ cleanup key
	, error "failed"
	)
  where
	move = checkDiskSpaceToGet key False $
		ifM (verifyKeyContent DefaultVerify UnVerified key src)
			( do
				moveAnnex key src
				return True
			, return False
			)

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
