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
	| LinkRemote

optParser :: CmdParamsDesc -> Parser (P2POpts, Maybe RemoteName)
optParser _ = (,)
	<$> (genaddresses <|> linkremote)
	<*> optional name
  where
	genaddresses = flag' GenAddresses
		( long "gen-addresses"
		<> help "generate addresses that allow accessing this repository over P2P networks"
		)
	linkremote = flag' LinkRemote
		( long "link"
		<> help "set up a P2P link to a git remote"
		)
	name = strOption
		( long "name"
		<> metavar paramName
		<> help "name of remote"
		)

seek :: (P2POpts, Maybe RemoteName) -> CommandSeek
seek (GenAddresses, _) = genAddresses =<< loadP2PAddresses
seek (LinkRemote, Just name) = commandAction $
	linkRemote (Git.Remote.makeLegalName name)
seek (LinkRemote, Nothing) = commandAction $
	linkRemote =<< unusedPeerRemoteName

unusedPeerRemoteName :: Annex RemoteName
unusedPeerRemoteName = go (1 :: Integer) =<< usednames
  where
	usednames = mapMaybe remoteName . remotes <$> Annex.gitRepo
	go n names = do
		let name = "peer" ++ show n
		if name `elem` names
			then go (n+1) names
			else return name

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
			Right (Just theiruuid) -> do
				ok <- inRepo $ Git.Command.runBool
					[ Param "remote", Param "add"
					, Param remotename
					, Param (formatP2PAddress addr)
					]
				when ok $ do
					storeUUIDIn (remoteConfig remotename "uuid") theiruuid
					storeP2PRemoteAuthToken addr authtoken
				return ok
			Right Nothing -> giveup "Unable to authenticate with peer. Please check the address and try again."
			Left e -> giveup $ "Unable to authenticate with peer: " ++ e
	connerror e = giveup $ "Unable to connect with peer. Please check that the peer is connected to the network, and try again. ("  ++ show e ++ ")"
