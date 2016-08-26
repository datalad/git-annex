{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.UUID (
	getUUID,
	getRepoUUID,
	getUncachedUUID,
	isUUIDConfigured,
	prepUUID,
	genUUID,
	genUUIDInNameSpace,
	gCryptNameSpace,
	removeRepoUUID,
	storeUUID,
	storeUUIDIn,
	setUUID,
	webUUID,
	bitTorrentUUID,
) where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Config
import Config

import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Data.UUID.V5 as U5
import Data.Bits.Utils

configkey :: ConfigKey
configkey = annexConfig "uuid"

{- Generates a random UUID, that does not include the MAC address. -}
genUUID :: IO UUID
genUUID = UUID . show <$> U4.nextRandom

{- Generates a UUID from a given string, using a namespace.
 - Given the same namespace, the same string will always result
 - in the same UUID. -}
genUUIDInNameSpace :: U.UUID -> String -> UUID
genUUIDInNameSpace namespace = UUID . show . U5.generateNamed namespace . s2w8

{- Namespace used for UUIDs derived from git-remote-gcrypt ids. -}
gCryptNameSpace :: U.UUID
gCryptNameSpace = U5.generateNamed U5.namespaceURL $
	s2w8 "http://git-annex.branchable.com/design/gcrypt/" 

{- Get current repository's UUID. -}
getUUID :: Annex UUID
getUUID = annexUUID <$> Annex.getGitConfig 

{- Looks up a remote repo's UUID, caching it in .git/config if
 - it's not already. -}
getRepoUUID :: Git.Repo -> Annex UUID
getRepoUUID r = do
	c <- toUUID <$> getConfig cachekey ""
	let u = getUncachedUUID r
	
	if c /= u && u /= NoUUID
		then do
			updatecache u
			return u
		else return c
  where
	updatecache u = do
		g <- gitRepo
		when (g /= r) $ storeUUIDIn cachekey u
	cachekey = remoteConfig r "uuid"

removeRepoUUID :: Annex ()
removeRepoUUID = do
	unsetConfig configkey
	storeUUID NoUUID

getUncachedUUID :: Git.Repo -> UUID
getUncachedUUID = toUUID . Git.Config.get key ""
  where
	(ConfigKey key) = configkey

-- Does the repo's config have a key for the UUID?
-- True even when the key has no value.
isUUIDConfigured :: Git.Repo -> Bool
isUUIDConfigured = isJust . Git.Config.getMaybe key 
  where
	(ConfigKey key) = configkey

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = whenM ((==) NoUUID <$> getUUID) $
	storeUUID =<< liftIO genUUID

storeUUID :: UUID -> Annex ()
storeUUID u = do
	Annex.changeGitConfig $ \c -> c { annexUUID = u }
	storeUUIDIn configkey u

storeUUIDIn :: ConfigKey -> UUID -> Annex ()
storeUUIDIn configfield = setConfig configfield . fromUUID

{- Only sets the configkey in the Repo; does not change .git/config -}
setUUID :: Git.Repo -> UUID -> IO Git.Repo
setUUID r u = do
	let s = show configkey ++ "=" ++ fromUUID u
	Git.Config.store s r

-- Dummy uuid for the whole web. Do not alter.
webUUID :: UUID
webUUID = UUID "00000000-0000-0000-0000-000000000001"

-- Dummy uuid for bittorrent. Do not alter.
bitTorrentUUID :: UUID
bitTorrentUUID = UUID "00000000-0000-0000-0000-000000000002"
