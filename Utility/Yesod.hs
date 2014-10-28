{- Yesod stuff, that's typically found in the scaffolded site.
 -
 - Also a bit of a compatability layer to make it easier to support yesod
 - 1.1-1.4 in the same code base.
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
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
#if ! MIN_VERSION_yesod(1,4,0)
	, withUrlRenderer
#endif
#if ! MIN_VERSION_yesod(1,2,0)
	, Html
#endif
	) where

#if MIN_VERSION_yesod(1,2,0)
import Yesod as Y
#else
import Yesod as Y hiding (Html)
#endif
#if MIN_VERSION_yesod_form(1,3,8)
import Yesod.Form.Bootstrap3 as Y hiding (bfs)
#else
import Assistant.WebApp.Bootstrap3 as Y hiding (bfs)
#endif
#ifndef __NO_TH__
import Yesod.Default.Util
import Language.Haskell.TH.Syntax (Q, Exp)
#if MIN_VERSION_yesod_default(1,1,0)
import Data.Default (def)
import Text.Hamlet hiding (Html)
#endif
#endif
#if ! MIN_VERSION_yesod(1,4,0)
#if MIN_VERSION_yesod(1,2,0)
import Data.Text (Text)
#endif
#endif

#ifndef __NO_TH__
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

hamletTemplate :: FilePath -> FilePath
hamletTemplate f = globFile "hamlet" f
#endif

{- Lift Handler to Widget -}
#if MIN_VERSION_yesod(1,2,0)
liftH :: Monad m => HandlerT site m a -> WidgetT site m a
liftH = handlerToWidget
#else
liftH :: MonadLift base m => base a -> m a
liftH = lift
#endif

{- Misc new names for stuff. -}
#if ! MIN_VERSION_yesod(1,2,0)
withUrlRenderer :: forall master sub. HtmlUrl (Route master) -> GHandler sub master RepHtml
withUrlRenderer = hamletToRepHtml

type Html = RepHtml
#else
#if ! MIN_VERSION_yesod_core(1,2,20)
withUrlRenderer :: MonadHandler m => ((Route (HandlerSite m) -> [(Text, Text)] -> Text) -> output) -> m output
withUrlRenderer = giveUrlRenderer
#endif
#endif
