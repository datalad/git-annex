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
import P2P.Annex
import Utility.AuthToken
import Git.Types
import qualified Git.Remote

cmd :: Command
cmd = command "p2p" SectionSetup
	"configure peer-2-peer links between repositories"
	paramNothing (seek <$$> optParser)

data P2POpts
	= GenAddresses
	| LinkRemote

data LinkDirection = BiDirectional | OneWay

optParser :: CmdParamsDesc -> Parser (P2POpts, Maybe RemoteName, LinkDirection)
optParser _ = (,,)
	<$> (genaddresses <|> linkremote)
	<*> optional name
	<*> direction
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
	direction = flag BiDirectional OneWay
		( long "one-way"
		<> help "make one-way link, rather than default bi-directional link"
		)

seek :: (P2POpts, Maybe RemoteName, LinkDirection) -> CommandSeek
seek (GenAddresses, _, _) = genAddresses =<< loadP2PAddresses
seek (LinkRemote, Just name, direction) = commandAction $
	linkRemote direction (Git.Remote.makeLegalName name)
seek (LinkRemote, Nothing, direction) = commandAction $
	linkRemote direction =<< unusedPeerRemoteName

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
linkRemote :: LinkDirection -> RemoteName -> CommandStart
linkRemote direction remotename = do
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
				Just addr -> do
					linkbackto <- case direction of
						OneWay -> return []
						BiDirectional -> do
							myaddrs <- loadP2PAddresses
							authtoken <- liftIO $ genAuthToken 128
							storeP2PAuthToken authtoken
							return $ map (`P2PAddressAuth` authtoken) myaddrs
					linkAddress addr linkbackto remotename
						>>= either giveup return
