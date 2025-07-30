{- git-remote-p2p-annex program
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitRemoteP2PAnnex where

import Common
import qualified Annex
import qualified Git.CurrentRepo
import P2P.Protocol
import P2P.IO
import Utility.AuthToken
import Annex.UUID
import P2P.Address
import P2P.Auth
import Annex.Action

run :: [String] -> IO ()
run = runner mkaddress
  where
	mkaddress address = 
		fromMaybe (error $ "unable to parse address: " ++ address) $
			unformatP2PAddress (p2pAnnexScheme ++ ":" ++ address)

runner :: (String -> P2PAddress) -> [String] -> IO ()
runner mkaddress (_remotename:saddress:[]) = forever $
	getLine >>= \case
		"capabilities" -> putStrLn "connect" >> ready
		"connect git-upload-pack" -> go UploadPack
		"connect git-receive-pack" -> go ReceivePack
		l -> giveup $ "gitremote-helpers protocol error at " ++ show l
  where
	address = mkaddress saddress
	go service = do
		ready
		connectService address service >>= \case
			Right exitcode -> exitWith exitcode
			Left e -> giveup $ describeProtoFailure e
	ready = do
		putStrLn ""
		hFlush stdout	
runner _ (_remotename:[]) = giveup "remote address not configured"
runner _ _ = giveup "expected remote name and address parameters"

connectService :: P2PAddress -> Service -> IO (Either ProtoFailure ExitCode)
connectService address service = do
	state <- Annex.new =<< Git.CurrentRepo.get
	Annex.eval state $ do
		authtoken <- fromMaybe nullAuthToken
			<$> loadP2PRemoteAuthToken address
		myuuid <- getUUID
		g <- Annex.gitRepo
		conn <- liftIO $ connectPeer (Just g) address
		runst <- liftIO $ mkRunState Client
		r <- liftIO $ runNetProto runst conn $ auth myuuid authtoken noop >>= \case
			Just _theiruuid -> connect service stdin stdout
			Nothing -> giveup $ "authentication failed, perhaps you need to set " ++ p2pAuthTokenEnv
		quiesce False
		return r
