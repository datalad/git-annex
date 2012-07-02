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
	, fieldCheck :: String -> IO Bool
	}

remoteUUID :: Field
remoteUUID = Field "remoteuuid" $
	-- does it look like a UUID?
	return . all (\c -> isAlphaNum c || c == '-')

associatedFile :: Field
associatedFile = Field "associatedfile" $ \value ->
	-- is the file located within the current directory?
	dirContains <$> getCurrentDirectory <*> pure value

getField :: Field -> Annex (Maybe String)
getField = Annex.getField . fieldName
