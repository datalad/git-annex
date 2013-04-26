{- git-annex assistant webapp form utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleContexts, TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Form where

import Types.Remote (RemoteConfigKey)

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
textField :: RenderMessage master FormMessage => Field sub master Text
textField = F.textField
	{ fieldView = \theId name attrs val _isReq -> [whamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="text" value="#{either id id val}">
|]
	}

{- Also without required attribute. -}
passwordField :: RenderMessage master FormMessage => Field sub master Text
passwordField = F.passwordField
	{ fieldView = \theId name attrs val _isReq -> toWidget [hamlet|
<input id="#{theId}" name="#{name}" *{attrs} type="password" value="#{either id id val}">
|]
	}

{- Makes a note widget be displayed after a field. -}
withNote :: Field sub master v -> GWidget sub master () -> Field sub master v
withNote field note = field { fieldView = newview }
  where
	newview theId name attrs val isReq = 
		let fieldwidget = (fieldView field) theId name attrs val isReq
		in [whamlet|^{fieldwidget}&nbsp;&nbsp;<span>^{note}</span>|]

{- Note that the toggle string must be unique on the form. -}
withExpandableNote :: Field sub master v -> (String, GWidget sub master ()) -> Field sub master v
withExpandableNote field (toggle, note) = withNote field expandablenote
  where
  	ident = "toggle_" ++ toggle
  	expandablenote = [whamlet|
<a .btn data-toggle="collapse" data-target="##{ident}">
  #{toggle}
<div ##{ident} .collapse>
  ^{note}
|]

data EnableEncryption = SharedEncryption | NoEncryption
	deriving (Eq)

{- Adds a check box to an AForm to control encryption. -}
enableEncryptionField :: RenderMessage master FormMessage => AForm sub master EnableEncryption
enableEncryptionField = areq (selectFieldList choices) "Encryption" (Just SharedEncryption)
  where
	choices :: [(Text, EnableEncryption)]
	choices =
		[ ("Encrypt all data", SharedEncryption)
		, ("Disable encryption", NoEncryption)
		]

{- Generates Remote configuration for encryption. -}
configureEncryption :: EnableEncryption -> (RemoteConfigKey, String)
configureEncryption SharedEncryption = ("encryption", "shared")
configureEncryption NoEncryption = ("encryption", "none")
