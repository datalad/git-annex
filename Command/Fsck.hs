{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory
import Data.List
import System.Posix.Files

import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import UUID
import Types
import Messages
import Utility
import Content
import LocationLog
import Locations
import Trust
import Utility.DataUnits
import Config

command :: [Command]
command = [repoCommand "fsck" (paramOptional $ paramRepeating paramPath) seek
	"check for problems"]

seek :: [CommandSeek]
seek = [withAttrFilesInGit "annex.numcopies" start]

start :: CommandStartAttrFile
start (file, attr) = notBareRepo $ isAnnexed file $ \(key, backend) -> do
	showStart "fsck" file
	next $ perform key file backend numcopies
	where
		numcopies = readMaybe attr :: Maybe Int

perform :: Key -> FilePath -> Backend Annex -> Maybe Int -> CommandPerform
perform key file backend numcopies = do
	-- the location log is checked first, so that if it has bad data
	-- that gets corrected
	locationlogok <- verifyLocationLog key file
	backendok <- fsckKey backend key (Just file) numcopies
	if locationlogok && backendok
		then next $ return True
		else stop

{- Checks that the location log reflects the current status of the key,
   in this repository only. -}
verifyLocationLog :: Key -> FilePath -> Annex Bool
verifyLocationLog key file = do
	g <- Annex.gitRepo
	present <- inAnnex key
	
	-- Since we're checking that a key's file is present, throw
	-- in a permission fixup here too.
	when present $ liftIO $ do
		let f = gitAnnexLocation g key
		preventWrite f
		preventWrite (parentDir f)

	u <- getUUID g
        uuids <- keyLocations key

	case (present, u `elem` uuids) of
		(True, False) -> do
				fix g u InfoPresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix g u InfoMissing
				warning $
					"** Based on the location log, " ++ file
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
	
	where
		fix g u s = do
			showNote "fixing location log"
			logChange g key u s

{- Checks a key for problems. -}
fsckKey :: Backend Annex -> Key -> Maybe FilePath -> Maybe Int -> Annex Bool
fsckKey backend key file numcopies = do
	size_ok <- checkKeySize key
	copies_ok <- checkKeyNumCopies key file numcopies
	backend_ok <- (Types.Backend.fsckKey backend) key
	return $ size_ok && copies_ok && backend_ok

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available. -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = do
	g <- Annex.gitRepo
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


checkKeyNumCopies :: Key -> Maybe FilePath -> Maybe Int -> Annex Bool
checkKeyNumCopies key file numcopies = do
	needed <- getNumCopies numcopies
	locations <- keyLocations key
	untrusted <- trustGet UnTrusted
	let untrustedlocations = intersect untrusted locations
	let safelocations = filter (`notElem` untrusted) locations
	let present = length safelocations
	if present < needed
		then do
			ppuuids <- Remote.prettyPrintUUIDs untrustedlocations
			warning $ missingNote (filename file key) present needed ppuuids
			return False
		else return True
	where
		filename Nothing k = show k
		filename (Just f) _ = f

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
