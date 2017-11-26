{- git-annex command
 -
 - Copyright 2011, 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Dead where

import Command
import Types.TrustLevel
import Command.Trust (trustCommand)
import Logs.Location
import Remote (keyLocations)
import Git.Types

cmd :: Command
cmd = command "dead" SectionSetup "hide a lost repository or key"
	(paramRepeating paramRemote) (seek <$$> optParser)

data DeadOptions = DeadRemotes [RemoteName] | DeadKeys [Key]

optParser :: CmdParamsDesc -> Parser DeadOptions
optParser desc = (DeadRemotes <$> cmdParams desc)
	<|> (DeadKeys <$> many (option (str >>= parseKey)
		( long "key" <> metavar paramKey
		<> help "keys whose content has been irretrievably lost"
		)))

seek :: DeadOptions -> CommandSeek
seek (DeadRemotes rs) = trustCommand "dead" DeadTrusted rs
seek (DeadKeys ks) = seekActions $ pure $ map startKey ks

startKey :: Key -> CommandStart
startKey key = do
	showStart "dead" (key2file key)
	ls <- keyLocations key
	case ls of
		[] -> next $ performKey key
		_ -> giveup "This key is still known to be present in some locations; not marking as dead."
		
performKey :: Key -> CommandPerform
performKey key = do
	setDead key
	next $ return True
