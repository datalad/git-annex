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

module Logs.UUID (
	describeUUID,
	recordUUID,
	uuidMap
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import qualified Annex.Branch
import Logs.UUIDBased
import qualified Annex.UUID

{- Filename of uuid.log. -}
logfile :: FilePath
logfile = "uuid.log"

{- Records a description for a uuid in the log. -}
describeUUID :: UUID -> String -> Annex ()
describeUUID uuid desc = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change logfile $
		showLog id . changeLog ts uuid desc . parseLog Just

{- Records the uuid in the log, if it's not already there. -}
recordUUID :: UUID -> Annex ()
recordUUID u = go . M.lookup u =<< uuidMap 
	where
		go (Just "") = set
		go Nothing = set
		go _ = return ()
		set = describeUUID u ""

{- Read the uuidLog into a simple Map.
 -
 - The UUID of the current repository is included explicitly, since
 - it may not have been described and so otherwise would not appear. -}
uuidMap :: Annex (M.Map UUID String)
uuidMap = do
	m <- (simpleMap . parseLog Just) <$> Annex.Branch.get logfile
	u <- Annex.UUID.getUUID
	return $ M.insertWith' preferold u "" m
	where
		preferold = flip const
