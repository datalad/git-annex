{- git-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Satisfy (cmd) where

import Command
import Command.Sync hiding (cmd)

cmd :: Command
cmd = withAnnexOptions [jobsOption, backendOption] $
	command "satisfy" SectionCommon 
		"transfer and drop content as configured"
		(paramRepeating paramRemote) (seek <--< optParser SatisfyMode)
