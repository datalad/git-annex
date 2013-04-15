{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Yesod where

import Yesod.Default.Util
import Language.Haskell.TH.Syntax
#if MIN_VERSION_yesod_default(1,1,0)
import Data.Default (def)
import Text.Hamlet
#endif

#ifndef __ANDROID__
widgetFile :: String -> Q Exp
#if ! MIN_VERSION_yesod_default(1,1,0)
widgetFile = widgetFileNoReload
#else
widgetFile = widgetFileNoReload $ def
	{ wfsHamletSettings = defaultHamletSettings
		{ hamletNewlines = AlwaysNewlines
		}
	}
#endif
#endif

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f
