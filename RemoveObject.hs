{-# LANGUAGE OverloadedStrings #-}

import qualified Aws
import qualified Aws.S3 as S3
import           Data.Conduit (($$+-))
import           Data.Conduit.Binary (sinkFile)
import           Network.HTTP.Conduit (withManager, responseBody)
import Control.Monad.IO.Class

main :: IO ()
main = do
  Just creds <- Aws.loadCredentialsFromEnv
  let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug)
  let s3cfg = Aws.defServiceConfig :: S3.S3Configuration Aws.NormalQuery

  withManager $ \mgr -> do
    rsp <- Aws.pureAws cfg s3cfg mgr $
        S3.DeleteObject "cloud-remote.pdf" "joeyh-test"
    liftIO $ print "removal done"
