{- git-annex-shell fields
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell.Fields where

import Common.Annex
import qualified Annex
import Git.FilePath

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

associatedFile :: Field
associatedFile = Field "associatedfile" $ \f ->
	-- is the file a safe relative filename?
	not (absoluteGitPath f) && not ("../" `isPrefixOf` f)

direct :: Field
direct = Field "direct" $ \f -> f == "1"

autoInit :: Field
autoInit = Field "autoinit" $ \f -> f == "1"
