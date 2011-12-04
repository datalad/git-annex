{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.UUID (
	getUUID,
	getRepoUUID,
	getUncachedUUID,
	prepUUID,
	genUUID
) where

import Common.Annex
import qualified Git
import qualified Build.SysConfig as SysConfig
import Config

configkey :: String
configkey = "annex.uuid"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = pOpen ReadFromPipe command params $ liftM toUUID . hGetLine
	where
		command = SysConfig.uuid
		params = if command == "uuid"
			-- request a random uuid be generated
			then ["-m"]
			-- uuidgen generates random uuid by default
			else []

{- Get current repository's UUID. -}
getUUID :: Annex UUID
getUUID = getRepoUUID =<< gitRepo

{- Looks up a repo's UUID, caching it in .git/config if it's not already. -}
getRepoUUID :: Git.Repo -> Annex UUID
getRepoUUID r = do
	c <- fromRepo cached
	let u = getUncachedUUID r
	
	if c /= u && u /= NoUUID
		then do
			updatecache u
			return u
		else return c
	where
		cached = toUUID . Git.configGet cachekey ""
		updatecache u = do
			g <- gitRepo
			when (g /= r) $ storeUUID cachekey u
		cachekey = remoteConfig r "uuid"

getUncachedUUID :: Git.Repo -> UUID
getUncachedUUID = toUUID . Git.configGet configkey ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = whenM ((==) NoUUID <$> getUUID) $
	storeUUID configkey =<< liftIO genUUID

storeUUID :: String -> UUID -> Annex ()
storeUUID configfield = setConfig configfield . fromUUID
