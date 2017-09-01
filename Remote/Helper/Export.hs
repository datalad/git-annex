{- exports to remotes
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Export where

import Annex.Common
import Types.Remote

exportUnsupported :: ExportActions Annex
exportUnsupported = ExportActions
	{ exportSupported = return False
	, storeExport = \_ _ _ _ -> return False
	, retrieveExport = \_ _ _ _ -> return (False, UnVerified)
	, removeExport = \_ _ -> return False
	, checkPresentExport = \_ _ -> return False
	, renameExport = \_ _ _ -> return False
	}
