{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (withManager, RequestBody(..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Control.Monad.IO.Class
import Control.Concurrent
import System.Posix.Files
import System.IO
import Control.Applicative
import qualified Data.Text as T

main :: IO ()
main = do
  {- Set up AWS credentials and the default configuration. -}
  Just creds <- Aws.loadCredentialsFromEnv
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug)
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  {- Set up a ResourceT region with an available HTTP manager. -}
  withManager $ \mgr -> do
    let file ="cloud-remote.pdf"
    -- streams file content, without buffering more than 1k in memory!
    let streamer sink = withFile file ReadMode $ \h -> sink $ S.hGet h 1024
    b <- liftIO $ L.readFile file
    size <- liftIO $ (fromIntegral . fileSize <$> getFileStatus file :: IO Integer)
    rsp <- Aws.pureAws cfg s3cfg mgr $
        S3.putObject "joeyh-test" (T.pack file) (RequestBodyStream (fromInteger size) streamer)
    liftIO $ print rsp
