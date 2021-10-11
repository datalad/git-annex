{- git-annex-shell fields
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell.Fields where

import Annex.Common
import qualified Annex

import Data.Char

{- A field, stored in Annex state, with a value sanity checker. -}
data Field = Field
	{ fieldName :: String
	, fieldCheck :: String -> Bool
	}

getField :: Field -> Annex (Maybe String)
getField = Annex.getField . fieldName

remoteUUID :: Field
remoteUUID = Field "remoteuuid" $
	-- does it look like a UUID?
	all (\c -> isAlphaNum c || c == '-')

unlocked :: Field
unlocked = Field "unlocked" $ \f -> f == "1"

autoInit :: Field
autoInit = Field "autoinit" $ \f -> f == "1"
