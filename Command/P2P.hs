{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.P2P where

import Command
import Git.Types
import P2P.Address
import P2P.Auth
import Utility.AuthToken

cmd :: Command
cmd = command "p2p" SectionSetup
	"configure peer-2-peer links between repositories"
	paramNothing (seek <$$> optParser)

data P2POpts
	= GenAddresses
	| LinkRemote P2PAddressAuth RemoteName

optParser :: CmdParamsDesc -> Parser P2POpts 
optParser _ = genaddresses <|> linkremote
  where
	genaddresses = flag' GenAddresses
		( long "gen-addresses"
		<> help "generate addresses that allow accessing this repository over P2P networks"
		)
	linkremote = LinkRemote
		<$> option readaddr
			( long "link"
			<> metavar paramAddress
			<> help "address of the peer to link with"
			)
		<*> strOption
			( long "named"
			<> metavar paramRemote
			<> help "specify name to use for git remote"
			)
	readaddr = eitherReader $ maybe (Left "address parse error") Right
		. unformatP2PAddress

seek :: P2POpts -> CommandSeek
seek GenAddresses = do
	addrs <- loadP2PAddresses
	if null addrs
		then giveup "No P2P networks are currrently available."
		else do
			authtoken <- liftIO $ genAuthToken 128
			storeP2PAuthToken authtoken
			-- Only addresses are output to stdout, to allow
			-- scripting.
			earlyWarning "These addresses allow access to this git-annex repository. Only share them with people you trust with that access, using trusted communication channels!"
			liftIO $ putStr $ unlines $
				map formatP2PAddress $
				map (`P2PAddressAuth` authtoken) addrs
seek (LinkRemote addr name) = do
	
