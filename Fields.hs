{- git-annex fields
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Fields where

import Common.Annex
import qualified Annex

import Data.Char

{- A field, stored in Annex state, with a value sanity checker. -}
data Field = Field
	{ fieldName :: String
	, fieldCheck :: String -> Bool
	}

remoteUUID :: Field
remoteUUID = Field "remoteuuid" $
	-- does it look like a UUID?
	all (\c -> isAlphaNum c || c == '-')

associatedFile :: Field
associatedFile = Field "associatedfile" $ \f ->
	-- is the file a safe relative filename?
	not (isAbsolute f) && not ("../" `isPrefixOf` f)

getField :: Field -> Annex (Maybe String)
getField = Annex.getField . fieldName
