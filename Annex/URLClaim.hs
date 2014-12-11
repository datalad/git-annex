{- Url claim checking.
 -
 - Copyright 2013-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.URLClaim (
	URLClaim(..),
	urlClaim
) where

import Common.Annex
import Types.URLClaim
import Logs.Web
import Remote
import qualified Types.Remote as Remote

urlClaim :: URLString -> Annex (Remote, URLClaim)
urlClaim url = do
	rs <- remoteList
	-- The web special remote claims urls by default.
	let web = Prelude.head $ filter (\r -> uuid r == webUUID) rs
	fromMaybe (web, URLClaimed) <$> getM (\r -> ret r <$> checkclaim r) rs
  where
	checkclaim = maybe (pure Nothing) (flip id url) . Remote.claimUrl

	ret _ Nothing = Nothing
	ret r (Just c) = Just (r, c)
