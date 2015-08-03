{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Also a bit of a compatability layer to make it easier to support yesod
 - 1.1-1.4 in the same code base.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, RankNTypes, FlexibleContexts #-}

module Utility.Yesod 
	( module Y
	, liftH
#ifndef __NO_TH__
	, widgetFile
	, hamletTemplate
#endif
	) where

import Yesod as Y
import Yesod.Form.Bootstrap3 as Y hiding (bfs)
#ifndef __NO_TH__
import Yesod.Default.Util
import Language.Haskell.TH.Syntax (Q, Exp)
import Data.Default (def)
import Text.Hamlet hiding (Html)
#endif
#if ! MIN_VERSION_yesod(1,4,0)
import Data.Text (Text)
#endif

#ifndef __NO_TH__
widgetFile :: String -> Q Exp
widgetFile = widgetFileNoReload $ def
	{ wfsHamletSettings = defaultHamletSettings
		{ hamletNewlines = AlwaysNewlines
		}
	}

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f
#endif

{- Lift Handler to Widget -}
liftH :: Monad m => HandlerT site m a -> WidgetT site m a
liftH = handlerToWidget

#if ! MIN_VERSION_yesod_core(1,2,20)
withUrlRenderer :: MonadHandler m => ((Route (HandlerSite m) -> [(Text, Text)] -> Text) -> output) -> m output
withUrlRenderer = giveUrlRenderer
#endif
