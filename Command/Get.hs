{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Common.Annex
import Command
import qualified Remote
import Annex.Content
import qualified Command.Move
import Logs.Transfer
import Annex.Wanted

def :: [Command]
def = [withOptions [Command.Move.fromOption] $ command "get" paramPaths seek
	SectionCommon "make content of annexed files available"]

seek :: [CommandSeek]
seek = [withField Command.Move.fromOption Remote.byNameWithUUID $ \from ->
	withFilesInGit $ whenAnnexed $ start from]

start :: Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start from file (key, _) = stopUnless (not <$> inAnnex key) $
	stopUnless (checkAuto (numCopiesCheck file key (<) <||> wantGet False (Just file))) $ do
		case from of
			Nothing -> go $ perform key file
			Just src ->
				-- get --from = copy --from
				stopUnless (Command.Move.fromOk src key) $
					go $ Command.Move.fromPerform src False key file
  where
	go a = do
		showStart "get" file
		next a

perform :: Key -> FilePath -> CommandPerform
perform key file = stopUnless (getViaTmp key $ getKeyFile key file) $
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKeyFile :: Key -> FilePath -> FilePath -> Annex Bool
getKeyFile key file dest = dispatch =<< Remote.keyPossibilities key
  where
	dispatch [] = do
		showNote "not available"
		showlocs
		return False
	dispatch remotes = trycopy remotes remotes
	trycopy full [] = do
		Remote.showTriedRemotes full
		showlocs
		return False
	trycopy full (r:rs) =
		ifM (probablyPresent r)
			( docopy r (trycopy full rs)
			, trycopy full rs
			)
	showlocs = Remote.showLocations key [] $
		"No other repository is known to contain the file."
	-- This check is to avoid an ugly message if a remote is a
	-- drive that is not mounted.
	probablyPresent r
		| Remote.hasKeyCheap r =
			either (const False) id <$> Remote.hasKey r key
		| otherwise = return True
	docopy r continue = do
		ok <- download (Remote.uuid r) key (Just file) noRetry $ \p -> do
			showAction $ "from " ++ Remote.name r
			Remote.retrieveKeyFile r key (Just file) dest p
		if ok then return ok else continue
