{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Semitrust where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)

def :: [Command]
def = [command "semitrust" (paramRepeating paramRemote) seek
	SectionSetup "return repository to default trust level"]

seek :: CommandSeek
seek = trustCommand "semitrust" SemiTrusted
