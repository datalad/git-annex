{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.UUID (
	getUUID,
	getRepoUUID,
	getUncachedUUID,
	prepUUID,
	genUUID,
	removeRepoUUID,
	storeUUID,
) where

import Common.Annex
import qualified Git
import qualified Git.Config
import Config

import qualified Data.UUID as U
import System.Random

configkey :: ConfigKey
configkey = annexConfig "uuid"

{- Generates a random UUID, that does not include the MAC address. -}
genUUID :: IO UUID
genUUID = UUID . show <$> (randomIO :: IO U.UUID)

{- Get current repository's UUID. -}
getUUID :: Annex UUID
getUUID = getRepoUUID =<< gitRepo

{- Looks up a repo's UUID, caching it in .git/config if it's not already. -}
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
		when (g /= r) $ storeUUID cachekey u
	cachekey = remoteConfig r "uuid"

removeRepoUUID :: Annex ()
removeRepoUUID = unsetConfig configkey

getUncachedUUID :: Git.Repo -> UUID
getUncachedUUID = toUUID . Git.Config.get key ""
  where
	(ConfigKey key) = configkey

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = whenM ((==) NoUUID <$> getUUID) $
	storeUUID configkey =<< liftIO genUUID

storeUUID :: ConfigKey -> UUID -> Annex ()
storeUUID configfield = setConfig configfield . fromUUID
