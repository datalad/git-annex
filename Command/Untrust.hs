{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Untrust where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)

cmd :: Command
cmd = command "untrust" SectionSetup "do not trust a repository"
	(paramRepeating paramRemote) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = trustCommand "untrust" UnTrusted
