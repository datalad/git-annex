{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.P2PHttp where

import Command
import P2P.Http
import qualified P2P.Protocol as P2P
import Annex.Url

import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client.Streaming
import Control.Concurrent
import Control.Concurrent.STM

cmd :: Command
cmd = command "p2phttp" SectionPlumbing
	"communicate in P2P protocol over http"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek ["server"] = startConcurrency commandStages $
	withLocalP2PConnections $ \acquireconn -> liftIO $ do
		st <- mkP2PHttpServerState acquireconn
		Warp.run 8080 (p2pHttpApp st)
seek ["client"] = testCheckPresent

testKeepLocked = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	keeplocked <- liftIO newEmptyTMVarIO
	_ <- liftIO $ forkIO $ do
		print "running, press enter to drop lock"
		_ <- getLine
		atomically $ writeTMVar keeplocked False
	liftIO $ clientKeepLocked (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64UUID (toUUID ("lck" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		(B64UUID (toUUID ("su" :: String)))
		[]
		keeplocked

testCheckPresent = do
	mgr <- httpManager <$> getUrlOptions
	burl <- liftIO $ parseBaseUrl "http://localhost:8080/"
	res <- liftIO $ clientCheckPresent (mkClientEnv mgr burl)
		(P2P.ProtocolVersion 3)
		(B64Key (fromJust $ deserializeKey ("WORM--foo" :: String)))
		(B64UUID (toUUID ("cu" :: String)))
		(B64UUID (toUUID ("f11773f0-11e1-45b2-9805-06db16768efe" :: String)))
		[]
	liftIO $ print res
