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

#if MIN_VERSION_yesod(1,2,0)
import Yesod hiding (textField, passwordField)
import Yesod.Form.Fields as F
#else
import Yesod hiding (textField, passwordField, selectField, selectFieldList)
import Yesod.Form.Fields as F hiding (selectField, selectFieldList)
import Data.String (IsString (..))
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
#endif
import Data.Text (Text)
import Assistant.WebApp.Bootstrap3 hiding (bfs)

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

{- In older Yesod versions attrs is written into the <option> tag instead of the
 - surrounding <select>. This breaks the Bootstrap 3 layout of select fields as
 - it requires the "form-control" class on the <select> tag.
 - We need to change that to behave the same way as in newer versions.
 -}
#if ! MIN_VERSION_yesod(1,2,0)
selectFieldList :: (Eq a, RenderMessage master FormMessage, RenderMessage master msg) => [(msg, a)] -> Field sub master a
selectFieldList = selectField . optionsPairs

selectField :: (Eq a, RenderMessage master FormMessage) => GHandler sub master (OptionList a) -> Field sub master a
selectField = selectFieldHelper
	(\theId name attrs inside -> [whamlet|<select ##{theId} name=#{name} *{attrs}>^{inside}|]) -- outside
	(\_theId _name isSel -> [whamlet|<option value=none :isSel:selected>_{MsgSelectNone}|]) -- onOpt
	(\_theId _name _attrs value isSel text -> [whamlet|<option value=#{value} :isSel:selected>#{text}|]) -- inside

selectFieldHelper :: (Eq a, RenderMessage master FormMessage)
	=> (Text -> Text -> [(Text, Text)] -> GWidget sub master () -> GWidget sub master ())
	-> (Text -> Text -> Bool -> GWidget sub master ())
	-> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> GWidget sub master ())
	-> GHandler sub master (OptionList a) -> Field sub master a
selectFieldHelper outside onOpt inside opts' = Field
	{ fieldParse = \x -> do
		opts <- opts'
		return $ selectParser opts x
	, fieldView = \theId name attrs val isReq -> do
		opts <- fmap olOptions $ lift opts'
		outside theId name attrs $ do
			unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
			flip mapM_ opts $ \opt -> inside
				theId
				name
				((if isReq then (("required", "required"):) else id) attrs)
				(optionExternalValue opt)
				((render opts val) == optionExternalValue opt)
				(optionDisplay opt)
	}
  where
	render _ (Left _) = ""
	render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
	selectParser _ [] = Right Nothing
	selectParser opts (s:_) = case s of
		"" -> Right Nothing
		"none" -> Right Nothing
		x -> case olReadExternal opts x of
			Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
			Just y -> Right $ Just y
#endif

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
<a .btn .btn-default data-toggle="collapse" data-target="##{ident}">#{toggle}</a>
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
enableEncryptionField = areq (selectFieldList choices) (bfs "Encryption") (Just SharedEncryption)
  where
	choices :: [(Text, EnableEncryption)]
	choices =
		[ ("Encrypt all data", SharedEncryption)
		, ("Disable encryption", NoEncryption)
		]

{- Defines the layout used by the Bootstrap3 form helper -}
bootstrapFormLayout :: BootstrapFormLayout
bootstrapFormLayout = BootstrapHorizontalForm (ColSm 0) (ColSm 2) (ColSm 0) (ColSm 10)

{- Adds the form-control class used by Bootstrap3 for layout to a field
 - This is the same as Yesod.Form.Bootstrap3.bfs except it takes just a Text
 - parameter as I couldn't get the original bfs to compile due to type ambiguities.
 -}
bfs :: Text -> FieldSettings master
bfs msg = FieldSettings
	{ fsLabel = SomeMessage msg
	, fsName  = Nothing
	, fsId    = Nothing
	, fsAttrs = [("class", "form-control")]
	, fsTooltip = Nothing
	}
