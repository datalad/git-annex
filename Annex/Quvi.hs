{- quvi options for git-annex
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Annex.Quvi where

import Common.Annex
import qualified Annex
import Utility.Quvi
import Utility.Url

withQuviOptions :: forall a. Query a -> [CommandParam] -> URLString -> Annex a
withQuviOptions a ps url = do
	opts <- map Param . annexQuviOptions <$> Annex.getGitConfig
	liftIO $ a (ps++opts) url
