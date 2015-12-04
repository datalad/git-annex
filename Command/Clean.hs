{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Clean where

import Common.Annex
import Command
import Annex.Content
import Annex.Link
import Git.Types

cmd :: Command
cmd = dontCheck repoExists $
	command "clean" SectionPlumbing 
		"git clean filter"
		paramFile (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [file] = do
	error ("clean " ++ file)
start [] = error "clean filter run without filename; upgrade git"
start _ = error "clean filter passed multiple filenames"
