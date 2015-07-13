{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Required where

import Command
import Logs.PreferredContent
import qualified Command.Wanted

cmd :: Command
cmd = Command.Wanted.cmd' "required" "get or set required content expression"
	requiredContentMapRaw
	requiredContentSet
