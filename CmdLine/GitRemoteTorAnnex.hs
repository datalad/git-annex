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
import Utility.Tor
import Utility.AuthToken
import Annex.UUID
import P2P.Address
import P2P.Auth

run :: [String] -> IO ()
run (_remotename:address:[]) = forever $
	getLine >>= \case
		"capabilities" -> putStrLn "connect" >> ready
		"connect git-upload-pack" -> go UploadPack
		"connect git-receive-pack" -> go ReceivePack
		l -> error $ "git-remote-helpers protocol error at " ++ show l
  where
	(onionaddress, onionport)
		| '/' `elem` address = parseAddressPort $
			reverse $ takeWhile (/= '/') $ reverse address
		| otherwise = parseAddressPort address
	go service = do
		ready
		either giveup exitWith
			=<< connectService onionaddress onionport service
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

connectService :: OnionAddress -> OnionPort -> Service -> IO (Either String ExitCode)
connectService address port service = do
	state <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval state $ do
		authtoken <- fromMaybe nullAuthToken
			<$> loadP2PRemoteAuthToken (TorAnnex address port)
		myuuid <- getUUID
		g <- Annex.gitRepo
		conn <- liftIO $ connectPeer g (TorAnnex address port)
		liftIO $ runNetProto conn $ auth myuuid authtoken >>= \case
			Just _theiruuid -> connect service stdin stdout
			Nothing -> giveup $ "authentication failed, perhaps you need to set " ++ p2pAuthTokenEnv
