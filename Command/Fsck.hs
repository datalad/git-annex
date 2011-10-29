{- git-annex command
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Common.Annex
import Command
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import qualified Backend
import qualified Git
import Annex.Content
import Logs.Location
import Logs.Trust
import Annex.UUID
import Utility.DataUnits
import Utility.FileMode
import Config

def :: [Command]
def = [command "fsck" paramPaths seek "check for problems"]

seek :: [CommandSeek]
seek = [withNumCopies start, withBarePresentKeys startBare]

start :: FilePath -> Maybe Int -> CommandStart
start file numcopies = isAnnexed file $ \(key, backend) -> do
	showStart "fsck" file
	next $ perform key file backend numcopies

perform :: Key -> FilePath -> Backend Annex -> Maybe Int -> CommandPerform
perform key file backend numcopies = check
	-- order matters
	[ verifyLocationLog key file
	, checkKeySize key
	, checkKeyNumCopies key file numcopies
	, (Types.Backend.fsckKey backend) key
	]

{- To fsck a bare repository, fsck each key in the location log. -}
withBarePresentKeys :: (Key -> CommandStart) -> CommandSeek
withBarePresentKeys a params = do
	bare <- Git.repoIsLocalBare <$> gitRepo
	if bare
		then do
			unless (null params) $ do
				error "fsck should be run without parameters in a bare repository"
			liftM (map a) loggedKeys
		else return []

startBare :: Key -> CommandStart
startBare key = case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
	Nothing -> stop
	Just backend -> do
		showStart "fsck" (show key)
		next $ performBare key backend

{- Note that numcopies cannot be checked in a bare repository, because
 - getting the numcopies value requires a working copy with .gitattributes
 - files. -}
performBare :: Key -> Backend Annex -> CommandPerform
performBare key backend = check
	[ verifyLocationLog key (show key)
	, checkKeySize key
	, (Types.Backend.fsckKey backend) key
	]

check :: [Annex Bool] -> CommandPerform	
check s = sequence s >>= dispatch
	where
		dispatch vs
			| all (== True) vs = next $ return True
			| otherwise = stop

{- Checks that the location log reflects the current status of the key,
   in this repository only. -}
verifyLocationLog :: Key -> String -> Annex Bool
verifyLocationLog key desc = do
	g <- gitRepo
	present <- inAnnex key
	
	-- Since we're checking that a key's file is present, throw
	-- in a permission fixup here too.
	when present $ liftIO $ do
		let f = gitAnnexLocation g key
		preventWrite f
		preventWrite (parentDir f)

	u <- getUUID
        uuids <- keyLocations key

	case (present, u `elem` uuids) of
		(True, False) -> do
				fix g u InfoPresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix g u InfoMissing
				warning $
					"** Based on the location log, " ++ desc
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
	
	where
		fix g u s = do
			showNote "fixing location log"
			logChange g key u s

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available. -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = do
	g <- gitRepo
	let file = gitAnnexLocation g key
	present <- liftIO $ doesFileExist file
	case (present, Types.Key.keySize key) of
		(_, Nothing) -> return True
		(False, _) -> return True
		(True, Just size) -> do
			stat <- liftIO $ getFileStatus file
			let size' = fromIntegral (fileSize stat)
			if size == size'
				then return True
				else do
					dest <- moveBad key
					warning $ "Bad file size (" ++
						compareSizes storageUnits True size size' ++ 
						"); moved to " ++ dest
					return False


checkKeyNumCopies :: Key -> FilePath -> Maybe Int -> Annex Bool
checkKeyNumCopies key file numcopies = do
	needed <- getNumCopies numcopies
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< keyLocations key
	let present = length safelocations
	if present < needed
		then do
			ppuuids <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
			warning $ missingNote file present needed ppuuids
			return False
		else return True

missingNote :: String -> Int -> Int -> String -> String
missingNote file 0 _ [] = 
		"** No known copies exist of " ++ file
missingNote file 0 _ untrusted =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy."
missingNote file present needed [] =
		"Only " ++ show present ++ " of " ++ show needed ++ 
		" trustworthy copies exist of " ++ file ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted = 
		missingNote file present needed [] ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted
