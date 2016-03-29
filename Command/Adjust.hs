{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Adjust where

import Command
import Annex.AdjustedBranch
import Annex.Version

cmd :: Command
cmd = notBareRepo $ notDirect $
	command "adjust" SectionSetup "enter adjusted branch"
		paramNothing (seek <$$> optParser)

optParser :: CmdParamsDesc -> Parser Adjustment
optParser _ = 
	flag' UnlockAdjustment
		( long "unlock"
		<> help "unlock annexed files"
		)
	{- Not ready yet
	<|> flag' HideMissingAdjustment
		( long "hide-missing"
		<> help "omit annexed files whose content is not present"
		)
	-}

seek :: Adjustment -> CommandSeek
seek = commandAction . start

start :: Adjustment -> CommandStart
start adj = do
	unlessM versionSupportsAdjustedBranch $
		error "Adjusted branches are only supported in v6 or newer repositories."
	enterAdjustedBranch adj
	next $ next $ return True
