{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Semitrust where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "semitrust" SectionSetup 
		"return repository to default trust level"
		(paramRepeating paramRepository)
		(withParams' seek completeRemotes)

seek :: CmdParams -> CommandSeek
seek = trustCommand "semitrust" SemiTrusted
