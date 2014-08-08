{- Amazon Web Services common infrastructure.
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Remote.Helper.AWS where

import Common.Annex
import Creds

import qualified Aws
import qualified Aws.S3 as S3
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)
import Data.IORef

creds :: UUID -> CredPairStorage
creds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
	, credPairRemoteKey = Just "s3creds"
	}

genCredentials :: CredPair -> IO Aws.Credentials
genCredentials (keyid, secret) = Aws.Credentials
	<$> pure (encodeUtf8 (T.pack keyid))
	<*> pure (encodeUtf8 (T.pack secret))
	<*> newIORef []
	<*> pure Nothing

data Service = S3 | Glacier
	deriving (Eq)

type Region = Text

regionMap :: Service -> M.Map Text Region
regionMap = M.fromList . regionInfo

defaultRegion :: Service -> Region
defaultRegion = snd . Prelude.head . regionInfo

data ServiceRegion = BothRegion Region | S3Region Region | GlacierRegion Region

{- The "US" and "EU" names are used as location constraints when creating a
 - S3 bucket. -}
regionInfo :: Service -> [(Text, Region)]
regionInfo service = map (\(t, r) -> (t, fromServiceRegion r)) $ 
	filter (matchingService . snd) $
	concatMap (\(t, l) -> map (t,) l) regions
  where
	regions =
		[ ("US East (N. Virginia)", [S3Region "US", GlacierRegion "us-east-1"])
		, ("US West (Oregon)", [BothRegion "us-west-2"])
		, ("US West (N. California)", [BothRegion "us-west-1"])
		, ("EU (Ireland)", [S3Region "EU", GlacierRegion "eu-west-1"])
		, ("Asia Pacific (Singapore)", [S3Region "ap-southeast-1"])
		, ("Asia Pacific (Tokyo)", [BothRegion "ap-northeast-1"])
		, ("Asia Pacific (Sydney)", [S3Region "ap-southeast-2"])
		, ("South America (São Paulo)", [S3Region "sa-east-1"])
		]

	fromServiceRegion (BothRegion s) = s
	fromServiceRegion (S3Region s) = s
	fromServiceRegion (GlacierRegion s) = s

	matchingService (BothRegion _) = True
	matchingService (S3Region _) = service == S3
	matchingService (GlacierRegion _) = service == Glacier

s3HostName :: Region -> B.ByteString
s3HostName "US" = "s3.amazonaws.com"
s3HostName "EU" = "s3-eu-west-1.amazonaws.com"
s3HostName r = encodeUtf8 $ T.concat ["s3-", r, ".amazonaws.com"]

s3DefaultHost :: String
s3DefaultHost = "s3.amazonaws.com"

mkLocationConstraint :: Region -> S3.LocationConstraint
mkLocationConstraint "US" = S3.locationUsClassic
mkLocationConstraint r = r
