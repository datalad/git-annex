{- git-annex command
 -
 - Copyright 2011, 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Dead where

import Command
import Common.Annex
import qualified Annex
import Types.TrustLevel
import Types.Key
import Command.Trust (trustCommand)
import Logs.Location
import Remote (keyLocations)

cmd :: Command
cmd = withOptions [keyOption] $ 
	command "dead" SectionSetup "hide a lost repository or key"
		(paramRepeating paramRemote) (withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = maybe (trustCommand "dead" DeadTrusted ps) (flip seekKey ps)
	=<< Annex.getField "key"

seekKey :: String -> CmdParams -> CommandSeek
seekKey ks = case file2key ks of
	Nothing -> error "Invalid key"
	Just key -> withNothing (startKey key)

startKey :: Key -> CommandStart
startKey key = do
	showStart "dead" (key2file key)
	ls <- keyLocations key
	case ls of
		[] -> next $ performKey key
		_ -> error "This key is still known to be present in some locations; not marking as dead."
		
performKey :: Key -> CommandPerform
performKey key = do
	setDead key
	next $ return True
