{- git-annex assistant fsck configuration
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Fsck where

import Assistant.WebApp.Common

getConfigFsckR :: Handler Html
getConfigFsckR = page "Consistency checks" (Just Configuration) $ do
	error "TODO"
