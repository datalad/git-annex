{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

#if defined VERSION_yesod_default
#if ! MIN_VERSION_yesod_default(1,1,0)
#define WITH_OLD_YESOD
#endif
#endif

module Utility.Yesod where

import Yesod.Default.Util
import Language.Haskell.TH.Syntax
#ifndef WITH_OLD_YESOD
import Data.Default (def)
import Text.Hamlet
#endif

widgetFile :: String -> Q Exp
#ifdef WITH_OLD_YESOD
widgetFile = widgetFileNoReload
#else
widgetFile = widgetFileNoReload $ def
	{ wfsHamletSettings = defaultHamletSettings
		{ hamletNewlines = AlwaysNewlines
		}
	}
#endif

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f
