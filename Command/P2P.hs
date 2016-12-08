{- git-annex command
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.P2P where

import Command
import P2P.Address
import P2P.Auth
import P2P.IO
import qualified P2P.Protocol as P2P
import Utility.AuthToken
import Git.Types
import qualified Git.Remote
import qualified Git.Command
import qualified Annex
import Annex.UUID
import Config

cmd :: Command
cmd = command "p2p" SectionSetup
	"configure peer-2-peer links between repositories"
	paramNothing (seek <$$> optParser)

data P2POpts
	= GenAddresses
	| LinkRemote RemoteName

optParser :: CmdParamsDesc -> Parser P2POpts 
optParser _ = genaddresses <|> linkremote
  where
	genaddresses = flag' GenAddresses
		( long "gen-addresses"
		<> help "generate addresses that allow accessing this repository over P2P networks"
		)
	linkremote = LinkRemote <$> strOption
		( long "link"
		<> metavar paramRemote
		<> help "specify name to use for git remote"
		)

seek :: P2POpts -> CommandSeek
seek GenAddresses = genAddresses =<< loadP2PAddresses
seek (LinkRemote name) = commandAction $
	linkRemote (Git.Remote.makeLegalName name)

-- Only addresses are output to stdout, to allow scripting.
genAddresses :: [P2PAddress] -> Annex ()
genAddresses [] = giveup "No P2P networks are currrently available."
genAddresses addrs = do
	authtoken <- liftIO $ genAuthToken 128
	storeP2PAuthToken authtoken
	earlyWarning "These addresses allow access to this git-annex repository. Only share them with people you trust with that access, using trusted communication channels!"
	liftIO $ putStr $ unlines $
		map formatP2PAddress $
			map (`P2PAddressAuth` authtoken) addrs

-- Address is read from stdin, to avoid leaking it in shell history.
linkRemote :: RemoteName -> CommandStart
linkRemote remotename = do
	showStart "p2p link" remotename
	next $ next prompt
  where
	prompt = do
		liftIO $ putStrLn ""
		liftIO $ putStr "Enter peer address: "
		liftIO $ hFlush stdout
		s <- liftIO getLine
		if null s
			then do
				liftIO $ hPutStrLn stderr "Nothing entered, giving up."
				return False
			else case unformatP2PAddress s of
				Nothing -> do
					liftIO $ hPutStrLn stderr "Unable to parse that address, please check its format and try again."
					prompt
				Just addr -> setup addr
	setup (P2PAddressAuth addr authtoken) = do
		g <- Annex.gitRepo
		conn <- liftIO $ connectPeer g addr
			`catchNonAsync` connerror
		u <- getUUID
		v <- liftIO $ runNetProto conn $ P2P.auth u authtoken
		case v of
			Just (Just theiruuid) -> do
				ok <- inRepo $ Git.Command.runBool
					[ Param "remote", Param "add"
					, Param remotename
					, Param (formatP2PAddress addr)
					]
				when ok $ do
					storeUUIDIn (remoteConfig remotename "uuid") theiruuid
					storeP2PRemoteAuthToken addr authtoken
				return ok
			_ -> giveup "Unable to authenticate with peer. Please check the address and try again."
	connerror e = giveup $ "Unable to connect with peer. Please check that the peer is connected to the network, and try again. ("  ++ show e ++ ")"
