{- git-annex uuids
 -
 - Each git repository used by git-annex has an annex.uuid setting that
 - uniquely identifies that repository.
 -
 - UUIDs of remotes are cached in git config, using keys named
 - remote.<name>.annex-uuid
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
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
	uuidMap,
	uuidLog
) where

import qualified Data.Map as M

import Annex.Common
import qualified Git
import qualified Annex.Branch
import Types.UUID
import qualified Build.SysConfig as SysConfig
import Config

configkey :: String
configkey = "annex.uuid"

{- Filename of uuid.log. -}
uuidLog :: FilePath
uuidLog = "uuid.log"

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

{- Looks up a repo's UUID. May return "" if none is known.
 -}
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
	when ("" == u) $ do
		uuid <- liftIO genUUID
		setConfig configkey uuid

{- Records a description for a uuid in the uuidLog. -}
describeUUID :: UUID -> String -> Annex ()
describeUUID uuid desc = Annex.Branch.change uuidLog $
	serialize . M.insert uuid desc . parse
	where
		serialize m = unlines $ map (\(u, d) -> u++" "++d) $ M.toList m

{- Read the uuidLog into a Map -}
uuidMap :: Annex (M.Map UUID String)
uuidMap = parse <$> Annex.Branch.get uuidLog

parse :: String -> M.Map UUID String
parse = M.fromList . map pair . lines
	where
		pair l
			| null ws = ("", "")
			| otherwise = (head ws, unwords $ drop 1 ws)
			where
				ws = words l
