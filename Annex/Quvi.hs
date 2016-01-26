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
import Utility.Quvi
import Utility.Url

withQuviOptions :: forall a. Query a -> [QuviParams] -> URLString -> Annex a
withQuviOptions a ps url = do
	v <- quviVersion
	opts <- map Param . annexQuviOptions <$> Annex.getGitConfig
	liftIO $ a v (concatMap (\mkp -> mkp v) ps ++ opts) url

quviSupported :: URLString -> Annex Bool
quviSupported u = liftIO . flip supported u =<< quviVersion

quviVersion :: Annex QuviVersion
quviVersion = go =<< Annex.getState Annex.quviversion
  where
	go (Just v) = return v
	go Nothing = do
		v <- liftIO probeVersion
		Annex.changeState $ \s -> s { Annex.quviversion = Just v }
		return v
