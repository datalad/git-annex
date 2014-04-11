{- git-annex assistant webapp form utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleContexts, TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Form where

import Assistant.WebApp.Types
import Assistant.Gpg

import Yesod hiding (textField, passwordField)
import Yesod.Form.Fields as F
import Data.Text (Text)

{- Yesod's textField sets the required attribute for required fields.
 - We don't want this, because many of the forms used in this webapp 
 - display a modal dialog when submitted, which interacts badly with
 - required field handling by the browser.
 -
 - Required fields are still checked by Yesod.
 -}
textField :: MkField Text
textField = F.textField
	{ fieldView = \theId name attrs val _isReq -> [whamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" value="#{either id id val}">
|]
	}

readonlyTextField :: MkField Text
readonlyTextField = F.textField
	{ fieldView = \theId name attrs val _isReq -> [whamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" value="#{either id id val}" readonly="true">
|]
	}

{- Also without required attribute. -}
passwordField :: MkField Text
passwordField = F.passwordField
	{ fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="password" value="#{either id id val}">
|]
	}

{- Makes a note widget be displayed after a field. -}
#if MIN_VERSION_yesod(1,2,0)
withNote :: (Monad m, ToWidget (HandlerSite m) a) => Field m v -> a -> Field m v
#else
withNote :: Field sub master v -> GWidget sub master () -> Field sub master v
#endif
withNote field note = field { fieldView = newview }
  where
	newview theId name attrs val isReq = 
		let fieldwidget = (fieldView field) theId name attrs val isReq
		in [whamlet|^{fieldwidget}&nbsp;&nbsp;<span>^{note}</span>|]

{- Note that the toggle string must be unique on the form. -}
#if MIN_VERSION_yesod(1,2,0)
withExpandableNote :: (Monad m, ToWidget (HandlerSite m) w) => Field m v -> (String, w) -> Field m v
#else
withExpandableNote :: Field sub master v -> (String, GWidget sub master ()) -> Field sub master v
#endif
withExpandableNote field (toggle, note) = withNote field $ [whamlet|
<a .btn data-toggle="collapse" data-target="##{ident}">#{toggle}</a>
<div ##{ident} .collapse>
  ^{note}
|]
  where
  	ident = "toggle_" ++ toggle

{- Adds a check box to an AForm to control encryption. -}
#if MIN_VERSION_yesod(1,2,0)
enableEncryptionField :: (RenderMessage site FormMessage) => AForm (HandlerT site IO) EnableEncryption
#else
enableEncryptionField :: RenderMessage master FormMessage => AForm sub master EnableEncryption
#endif
enableEncryptionField = areq (selectFieldList choices) "Encryption" (Just SharedEncryption)
  where
	choices :: [(Text, EnableEncryption)]
	choices =
		[ ("Encrypt all data", SharedEncryption)
		, ("Disable encryption", NoEncryption)
		]
