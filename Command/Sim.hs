{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Sim where

import Command
import Annex.Sim

cmd :: Command
cmd = command "sim" SectionTesting
	"simulate a network of repositories"
	paramCommand (withParams seek)

seek :: CmdParams -> CommandSeek
seek = undefined
