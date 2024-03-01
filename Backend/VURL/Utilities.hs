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
import Types.KeySource
import Logs.EquivilantKeys
import qualified Backend.Hash
import Utility.Metered

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
	| otherwise = do
		liftIO $ print ("migrateFromURL", inannex, fromKey keyVariety oldkey)
		return Nothing
  where
	-- Relies on the first hash being cryptographically secure, and the
	-- default hash used by git-annex.
	hashbackend = Prelude.head Backend.Hash.backends

-- The Backend must use a cryptographically secure hash.
generateEquivilantKey :: Backend -> RawFilePath -> Annex (Maybe Key)
generateEquivilantKey b f =
	case genKey b of
		Just genkey -> do
			showSideAction (UnquotedString Backend.Hash.descChecksum)
			Just <$> genkey source nullMeterUpdate
		Nothing -> return Nothing
  where
	source = KeySource
		{ keyFilename = mempty -- avoid adding any extension
		, contentLocation = f
		, inodeCache = Nothing
		}
