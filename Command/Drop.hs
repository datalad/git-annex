{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Drop where

import Common.Annex
import Command
import qualified Remote
import qualified Annex
import Annex.UUID
import Logs.Location
import Logs.Trust
import Annex.Content
import Config

def :: [Command]
def = [dontCheck fromOpt $ command "drop" paramPaths seek
	"indicate content of files not currently wanted"]

seek :: [CommandSeek]
seek = [withNumCopies start]

start :: FilePath -> Maybe Int -> CommandStart
start file numcopies = isAnnexed file $ \(key, _) ->
	autoCopies key (>) numcopies $ do
		from <- Annex.getState Annex.fromremote
		case from of
			Nothing -> startLocal file numcopies key
			Just name -> do
				remote <- Remote.byName name
				u <- getUUID
				if Remote.uuid remote == u
					then startLocal file numcopies key
					else startRemote file numcopies key remote

startLocal :: FilePath -> Maybe Int -> Key -> CommandStart
startLocal file numcopies key = do
	present <- inAnnex key
	if present
		then do
			showStart "drop" file
			next $ performLocal key numcopies
		else stop

startRemote :: FilePath -> Maybe Int -> Key -> Remote.Remote Annex -> CommandStart
startRemote file numcopies key remote = do
	showStart "drop" file
	next $ performRemote key numcopies remote

performLocal :: Key -> Maybe Int -> CommandPerform
performLocal key numcopies = lockExclusive key $ do
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	untrusteduuids <- trustGet UnTrusted
	let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids++untrusteduuids)
	success <- canDropKey key numcopies trusteduuids tocheck []
	if success
		then do
			whenM (inAnnex key) $ removeAnnex key
			next $ cleanupLocal key
		else stop

performRemote :: Key -> Maybe Int -> Remote.Remote Annex -> CommandPerform
performRemote key numcopies remote = lockExclusive key $ do
	-- Filter the remote it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	-- When the local repo has the key, that's one additional copy.
	(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
	present <- inAnnex key
	u <- getUUID
	let have = filter (/= uuid) $
		if present then u:trusteduuids else trusteduuids
	untrusteduuids <- trustGet UnTrusted
	let tocheck = filter (/= remote) $
		Remote.remotesWithoutUUID remotes (have++untrusteduuids)
	success <- canDropKey key numcopies have tocheck [uuid]
	if success
		then do
			ok <- Remote.removeKey remote key
			next $ cleanupRemote key remote ok
		else stop
	where
		uuid = Remote.uuid remote

cleanupLocal :: Key -> CommandCleanup
cleanupLocal key = do
	logStatus key InfoMissing
	return True

cleanupRemote :: Key -> Remote.Remote Annex -> Bool -> CommandCleanup
cleanupRemote key remote ok = do
	-- better safe than sorry: assume the remote dropped the key
	-- even if it seemed to fail; the failure could have occurred
	-- after it really dropped it
	Remote.remoteHasKey remote key False
	return ok

{- Checks specified remotes to verify that enough copies of a key exist to
 - allow it to be safely removed (with no data loss). Can be provided with
 - some locations where the key is known/assumed to be present. -}
canDropKey :: Key -> Maybe Int -> [UUID] -> [Remote.Remote Annex] -> [UUID] -> Annex Bool
canDropKey key numcopiesM have check skip = do
	force <- Annex.getState Annex.force
	if force || numcopiesM == Just 0
		then return True
		else do
			need <- getNumCopies numcopiesM
			findCopies key need skip have check

findCopies :: Key -> Int -> [UUID] -> [UUID] -> [Remote.Remote Annex] -> Annex Bool
findCopies key need skip = helper []
	where
		helper bad have []
			| length have >= need = return True
			| otherwise = notEnoughCopies key need have skip bad
		helper bad have (r:rs)
			| length have >= need = return True
			| otherwise = do
				let u = Remote.uuid r
				let duplicate = u `elem` have
				haskey <- Remote.hasKey r key
				case (duplicate, haskey) of
					(False, Right True) -> helper bad (u:have) rs
					(False, Left _)     -> helper (r:bad) have rs
					_                   -> helper bad have rs

notEnoughCopies :: Key -> Int -> [UUID] -> [UUID] -> [Remote.Remote Annex] -> Annex Bool
notEnoughCopies key need have skip bad = do
	unsafe
	showLongNote $
		"Could only verify the existence of " ++
		show (length have) ++ " out of " ++ show need ++ 
		" necessary copies"
	Remote.showTriedRemotes bad
	Remote.showLocations key (have++skip)
	hint
	return False
	where
		unsafe = showNote "unsafe"
		hint = showLongNote "(Use --force to override this check, or adjust annex.numcopies.)"
