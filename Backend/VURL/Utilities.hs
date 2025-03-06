{- git-annex VURL backend utilities
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.VURL.Utilities where

import Annex.Common
import Types.Key
import Types.Backend
import Logs.EquivilantKeys
import qualified Backend.Hash

migrateFromURLToVURL :: Key -> Backend -> AssociatedFile -> Bool -> Annex (Maybe Key)
migrateFromURLToVURL oldkey newbackend _af inannex
	| inannex && fromKey keyVariety oldkey == URLKey && backendVariety newbackend == VURLKey = do
		let newkey = mkKey $ const $
			(keyData oldkey)
				{ keyVariety = VURLKey }
		contentfile <- calcRepo (gitAnnexLocation oldkey)
		generateEquivilantKey hashbackend contentfile >>= \case
			Nothing -> return Nothing
			Just ek -> do
				setEquivilantKey newkey ek
				return (Just newkey)
	| otherwise = return Nothing
  where
	-- Relies on the first hash being cryptographically secure, and the
	-- default hash used by git-annex.
	hashbackend = fromMaybe (error "internal") $ 
		headMaybe Backend.Hash.backends

migrateFromVURLToURL :: Key -> Backend -> AssociatedFile -> Bool -> Annex (Maybe Key)
migrateFromVURLToURL oldkey newbackend _af _
	| fromKey keyVariety oldkey == VURLKey && backendVariety newbackend == URLKey =
		return $ Just $ mkKey $ const $
			(keyData oldkey)
				{ keyVariety = URLKey }
	| otherwise = return Nothing
