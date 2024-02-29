{- git-annex VURL backend -- like URL, but with hash-based verification
 - of transfers between git-annex repositories.
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.VURL (
	backends,
) where

import Annex.Common
import Types.Key
import Types.Backend
import Logs.EquivilantKeys
import Backend.Variety

backends :: [Backend]
backends = [backendVURL]

backendVURL :: Backend
backendVURL = Backend
	{ backendVariety = VURLKey
	, genKey = Nothing
	, verifyKeyContent = Nothing -- TODO
	, verifyKeyContentIncrementally = Nothing -- TODO
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	-- Even if a hash is recorded on initial download from the web and
	-- is used to verify every subsequent transfer including other
	-- downloads from the web, in a split-brain situation there
	-- can be more than one hash and different versions of the content.
	-- So the content is not stable.
	, isStableKey = const False
	-- Not all keys using this backend are necessarily 
	-- cryptographically secure.
	, isCryptographicallySecure = False
	-- A key is secure when all recorded equivilant keys are.
	-- If there are none recorded yet, it's secure because when
	-- downloaded, an equivilant key that is cryptographically secure
	-- will be constructed then.
	, isCryptographicallySecureKey = \k ->
		equivkeys k >>= \case
			[] -> return True
			l -> do
				let check ek = getbackend ek >>= \case
					Nothing -> pure False
					Just b -> isCryptographicallySecureKey b ek
				allM check l
	}
  where
	equivkeys k = filter allowedequiv <$> getEquivilantKeys k
	-- Don't allow using VURL keys as equivilant keys, because that
	-- could let a crafted git-annex branch cause an infinite loop.
	allowedequiv ek = fromKey keyVariety ek /= VURLKey
	varietymap = makeVarietyMap regularBackendList
	getbackend ek = maybeLookupBackendVarietyMap (fromKey keyVariety ek) varietymap

