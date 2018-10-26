{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Also a bit of a compatability layer for older versions of yesod.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, RankNTypes, FlexibleContexts #-}

module Utility.Yesod 
	( module Y
	, liftH
	, widgetFile
	, hamletTemplate
	) where

import Yesod as Y
import Yesod.Form.Bootstrap3 as Y hiding (bfs)
import Yesod.Default.Util
import Language.Haskell.TH.Syntax (Q, Exp)
import Data.Default (def)
import Text.Hamlet hiding (Html)

widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload $ def
	{ wfsHamletSettings = defaultHamletSettings
		{ hamletNewlines = AlwaysNewlines
		}
	}

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f

{- Lift Handler to Widget -}
#if MIN_VERSION_yesod_core(1,6,0)
liftH :: HandlerFor site a -> WidgetFor site a 
#else
liftH :: Monad m => HandlerT site m a -> WidgetT site m a
#endif
liftH = handlerToWidget
