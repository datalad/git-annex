{- Amazon Web Services common infrastructure.
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Remote.Helper.AWS where

import Common.Annex
import Creds

import qualified Data.Map as M
import Data.Text (Text)

creds :: UUID -> CredPairStorage
creds u = CredPairStorage
	{ credPairFile = fromUUID u
	, credPairEnvironment = ("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY")
	, credPairRemoteKey = Just "s3creds"
	}

data Service = S3 | Glacier
	deriving (Eq)

type Region = Text

regionMap :: Service -> M.Map Text Region
regionMap = M.fromList . regionInfo

defaultRegion :: Service -> Region
defaultRegion = snd . Prelude.head . regionInfo

{- S3 and Glacier use different names for some regions. Ie, "us-east-1"
 - cannot be used with S3, while "US" cannot be used with Glacier. Dunno why.
 - Also, Glacier is not yet available in all regions. -}
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

data ServiceRegion = BothRegion Region | S3Region Region | GlacierRegion Region
