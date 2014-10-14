{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Untrust where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)

cmd :: [Command]
cmd = [command "untrust" (paramRepeating paramRemote) seek
	SectionSetup "do not trust a repository"]

seek :: CommandSeek
seek = trustCommand "untrust" UnTrusted
