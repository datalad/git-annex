{- quvi options for git-annex
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Annex.Quvi where

import Annex.Common
import qualified Annex
import Annex.Url
import Utility.Quvi

withQuviOptions :: forall a. Query a -> [QuviParams] -> URLString -> Annex a
withQuviOptions a ps url = do
	v <- quviVersion
	opts <- map Param . annexQuviOptions <$> Annex.getGitConfig
	liftIO $ a v (concatMap (\mkp -> mkp v) ps ++ opts) url

quviSupported :: URLString -> Annex Bool
quviSupported u = ifM httpAddressesUnlimited
	( liftIO . flip supported u =<< quviVersion
	-- Don't allow any url schemes to be used when
	-- there's a limit on the allowed addresses, because
	-- there is no way to prevent quvi from
	-- redirecting to any address.
	, return False
	)

quviVersion :: Annex QuviVersion
quviVersion = go =<< Annex.getState Annex.quviversion
  where
	go (Just v) = return v
	go Nothing = do
		v <- liftIO probeVersion
		Annex.changeState $ \s -> s { Annex.quviversion = Just v }
		return v
