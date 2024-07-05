{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase    #-}
module Main (main) where

import           Control.Concurrent
                 (threadDelay)
import           Control.Monad.IO.Class
                 (MonadIO (..))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import           Data.Maybe
                 (fromMaybe)
import           Network.HTTP.Client
                 (defaultManagerSettings, newManager)
import           Network.Wai
                 (Application)
import           System.Environment
                 (getArgs, lookupEnv)
import           Text.Read
                 (readMaybe)

import           Servant
import           Servant.Client.Streaming
import qualified Servant.Types.SourceT as S
import Control.Concurrent.MVar
import System.IO.Unsafe

import qualified Network.Wai.Handler.Warp     as Warp

type API = "readme" :> StreamGet NoFraming OctetStream (SourceIO BS.ByteString)

api :: Proxy API
api = Proxy

server :: Server API
server = readme where
    readme = liftIO $ do
        putStrLn "/proxy"
        return $ S.SourceT $ \k -> do
		k =<< readfilelazy "README.md"
		k =<< readfilelazy "another"

app :: Application
app = serve api server

cli :: ClientM (S.SourceT IO BS.ByteString)
cli = client api

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("server":_) -> do
            putStrLn "Starting cookbook-basic-streaming at http://localhost:8000"
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        ("client":ns:_) -> do
            mgr <- newManager defaultManagerSettings
            burl <- parseBaseUrl "http://localhost:8000/"
            withClientM cli (mkClientEnv mgr burl) $ \me -> case me of
                Left err  -> print err
                Right src -> do
                	b <- S.unSourceT src gatherbytestring
			liftIO $ print "got it all, writing"
			BL.writeFile "got" (BL.init b)
        _ -> do
            putStrLn "Try:"
            putStrLn "cabal new-run cookbook-basic-streaming server"
            putStrLn "cabal new-run cookbook-basic-streaming client 10"
            putStrLn "time curl -H 'Accept: application/json' localhost:8000/slow/5"

readfilelazy :: FilePath -> IO (S.StepT IO BS.ByteString)
readfilelazy file = do
	bl <- BL.readFile file
	v <- newMVar (BL.toChunks bl)
 	return (go v)
  where
    go :: MVar [BS.ByteString] -> S.StepT IO BS.ByteString
    go v = S.fromActionStep BS.null $ do
	print "chunk"
	modifyMVar v $ pure . \case
		[] -> ([], BS.empty)
		(b:bs) -> (bs, b)

gatherbytestring :: S.StepT IO BS.ByteString -> IO BL.ByteString
gatherbytestring x = do
	l <- unsafeInterleaveIO $ go x
	return l
  where
	go S.Stop        = return BLI.Empty
	go (S.Error err) = error $ show ("ERROR", err)
	go (S.Skip s)    = do
		go s
	go (S.Effect ms) = do
		ms >>= go
	go (S.Yield v s) = do
		BLI.Chunk v <$> unsafeInterleaveIO (go s)

