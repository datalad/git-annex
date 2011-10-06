{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - uuid.log stores a list of known uuids, and their descriptions.
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module UUID (
	UUID,
	getUUID,
	getUncachedUUID,
	prepUUID,
	genUUID,
	describeUUID,
	uuidMap
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import qualified Git
import qualified Annex.Branch
import Types.UUID
import qualified Build.SysConfig as SysConfig
import Config
import UUIDLog

configkey :: String
configkey = "annex.uuid"

{- Filename of uuid.log. -}
logfile :: FilePath
logfile = "uuid.log"

{- Generates a UUID. There is a library for this, but it's not packaged,
 - so use the command line tool. -}
genUUID :: IO UUID
genUUID = liftIO $ pOpen ReadFromPipe command params $ \h -> hGetLine h
	where
		command = SysConfig.uuid
		params = if command == "uuid"
			-- request a random uuid be generated
			then ["-m"]
			-- uuidgen generates random uuid by default
			else []

{- Looks up a repo's UUID. May return "" if none is known. -}
getUUID :: Git.Repo -> Annex UUID
getUUID r = do
	g <- gitRepo

	let c = cached g
	let u = getUncachedUUID r
	
	if c /= u && u /= ""
		then do
			updatecache g u
			return u
		else return c
	where
		cached g = Git.configGet g cachekey ""
		updatecache g u = when (g /= r) $ setConfig cachekey u
		cachekey = "remote." ++ fromMaybe "" (Git.repoRemoteName r) ++ ".annex-uuid"

getUncachedUUID :: Git.Repo -> UUID
getUncachedUUID r = Git.configGet r configkey ""

{- Make sure that the repo has an annex.uuid setting. -}
prepUUID :: Annex ()
prepUUID = do
	u <- getUUID =<< gitRepo
	when (null u) $ do
		uuid <- liftIO genUUID
		setConfig configkey uuid

{- Records a description for a uuid in the log. -}
describeUUID :: UUID -> String -> Annex ()
describeUUID uuid desc = do
	ts <- liftIO $ getPOSIXTime
	Annex.Branch.change logfile $
		showLog id . changeLog ts uuid desc . parseLog Just

{- Read the uuidLog into a simple Map -}
uuidMap :: Annex (M.Map UUID String)
uuidMap = (simpleMap . parseLog Just) <$> Annex.Branch.get logfile
