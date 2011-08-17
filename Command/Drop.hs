{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Drop where

import Command
import qualified Remote
import qualified Annex
import LocationLog
import Types
import Content
import Messages
import Utility
import Trust
import Config

command :: [Command]
command = [repoCommand "drop" paramPath seek
	"indicate content of files not currently wanted"]

seek :: [CommandSeek]
seek = [withAttrFilesInGit "annex.numcopies" start]

{- Indicates a file's content is not wanted anymore, and should be removed
 - if it's safe to do so. -}
start :: CommandStartAttrFile
start (file, attr) = isAnnexed file $ \(key, _) -> do
	present <- inAnnex key
	if present
		then do
			showStart "drop" file
			next $ perform key numcopies
		else stop
	where
		numcopies = readMaybe attr :: Maybe Int

perform :: Key -> Maybe Int -> CommandPerform
perform key numcopies = do
	success <- dropKey key numcopies
	if success
		then next $ cleanup key
		else stop

cleanup :: Key -> CommandCleanup
cleanup key = do
	whenM (inAnnex key) $ removeAnnex key
	logStatus key InfoMissing
	return True

{- Checks remotes to verify that enough copies of a key exist to allow
 - for a key to be safely removed (with no data loss), and fails with an
 - error if not. -}
dropKey :: Key -> Maybe Int -> Annex Bool
dropKey key numcopiesM = do
	force <- Annex.getState Annex.force
	if force || numcopiesM == Just 0
		then return True
		else do
			(remotes, trusteduuids) <- Remote.keyPossibilitiesTrusted key
			untrusteduuids <- trustGet UnTrusted
			let tocheck = Remote.remotesWithoutUUID remotes (trusteduuids++untrusteduuids)
			numcopies <- getNumCopies numcopiesM
			findcopies numcopies trusteduuids tocheck []
	where
		findcopies need have [] bad
			| length have >= need = return True
			| otherwise = notEnoughCopies need have bad
		findcopies need have (r:rs) bad
			| length have >= need = return True
			| otherwise = do
				let u = Remote.uuid r
				let dup = u `elem` have
				haskey <- Remote.hasKey r key
				case (dup, haskey) of
					(False, Right True)	-> findcopies need (u:have) rs bad
					(False, Left _)		-> findcopies need have rs (r:bad)
					_			-> findcopies need have rs bad
		notEnoughCopies need have bad = do
			unsafe
			showLongNote $
				"Could only verify the existence of " ++
				show (length have) ++ " out of " ++ show need ++ 
				" necessary copies"
			Remote.showTriedRemotes bad
			Remote.showLocations key have
			hint
			return False
		unsafe = showNote "unsafe"
		hint = showLongNote "(Use --force to override this check, or adjust annex.numcopies.)"
