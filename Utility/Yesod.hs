{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Yesod where

import Yesod.Default.Util
import Language.Haskell.TH.Syntax

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f
