{- git-remote-tor-annex program
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitRemoteTorAnnex where

import Common
import qualified Annex
import qualified Git.CurrentRepo
import P2P.Protocol
import P2P.IO
import Remote.Helper.Tor
import Utility.Tor
import Utility.AuthToken
import Annex.UUID
import P2P.Address
import P2P.Auth

run :: [String] -> IO ()
run (_remotename:address:[]) = forever $ do
	-- gitremote-helpers protocol
	l <- getLine
	case l of
		"capabilities" -> putStrLn "connect" >> ready
		"connect git-upload-pack" -> go UploadPack
		"connect git-receive-pack" -> go ReceivePack
		_ -> error $ "git-remote-helpers protocol error at " ++ show l
  where
	(onionaddress, onionport)
		| '/' `elem` address = parseAddressPort $
			reverse $ takeWhile (/= '/') $ reverse address
		| otherwise = parseAddressPort address
	go service = do
		ready
		res <- connectService onionaddress onionport service 
		exitWith (fromMaybe (ExitFailure 1) res)
	ready = do
		putStrLn ""
		hFlush stdout
		
run (_remotename:[]) = giveup "remote address not configured"
run _ = giveup "expected remote name and address parameters"

parseAddressPort :: String -> (OnionAddress, OnionPort)
parseAddressPort s = 
	let (a, sp) = separate (== ':') s
	in case readish sp of
		Nothing -> giveup "onion address must include port number"
		Just p -> (OnionAddress a, p)

connectService :: OnionAddress -> OnionPort -> Service -> IO (Maybe ExitCode)
connectService address port service = do
	state <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval state $ do
		authtoken <- fromMaybe nullAuthToken
			<$> loadP2PRemoteAuthToken (TorAnnex address port)
		myuuid <- getUUID
		g <- Annex.gitRepo
		h <- liftIO $ torHandle =<< connectHiddenService address port
		let runenv = RunEnv
			{ runRepo = g
			, runCheckAuth = const False
			, runIhdl = h
			, runOhdl = h
			}
		liftIO $ runNetProto runenv $ do
			v <- auth myuuid authtoken
			case v of
				Just _theiruuid -> connect service stdin stdout
				Nothing -> giveup $ "authentication failed, perhaps you need to set " ++ p2pAuthTokenEnv
