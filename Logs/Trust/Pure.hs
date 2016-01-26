{- git-annex trust log, pure operations
 -
 - Copyright 2010-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Trust.Pure where

import Annex.Common
import Types.TrustLevel
import Logs.UUIDBased

calcTrustMap :: String -> TrustMap
calcTrustMap = simpleMap . parseLog (Just . parseTrustLog)

{- The trust.log used to only list trusted repos, without a field for the
 - trust status, which is why this defaults to Trusted. -}
parseTrustLog :: String -> TrustLevel
parseTrustLog s = maybe Trusted parse $ headMaybe $ words s
  where
	parse "1" = Trusted
	parse "0" = UnTrusted
	parse "X" = DeadTrusted
	parse _ = SemiTrusted

showTrustLog :: TrustLevel -> String
showTrustLog Trusted = "1"
showTrustLog UnTrusted = "0"
showTrustLog DeadTrusted = "X"
showTrustLog SemiTrusted = "?"

prop_parse_show_TrustLog :: Bool
prop_parse_show_TrustLog = all check [minBound .. maxBound]
  where
	check l = parseTrustLog (showTrustLog l) == l
