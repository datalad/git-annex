{- webapp url renderer access from the assistant
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Types.UrlRenderer (
	UrlRenderer,
	newUrlRenderer
) where

#ifdef WITH_WEBAPP

import Assistant.WebApp (UrlRenderer, newUrlRenderer)

#else

data UrlRenderer = UrlRenderer -- dummy type

newUrlRenderer :: IO UrlRenderer
newUrlRenderer = return UrlRenderer

#endif
