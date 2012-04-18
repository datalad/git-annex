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

def :: [Command]
def = [withOptions [Command.Move.fromOption] $ command "get" paramPaths seek
	"make content of annexed files available"]

seek :: [CommandSeek]
seek = [withField Command.Move.fromOption Remote.byName $ \from ->
	withFilesInGit $ whenAnnexed $ start from]

start :: Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start from file (key, _) = stopUnless (not <$> inAnnex key) $
	autoCopies file key (<) $ \_numcopies ->
		case from of
			Nothing -> go $ perform key
			Just src -> do
				-- get --from = copy --from
				stopUnless (Command.Move.fromOk src key) $
					go $ Command.Move.fromPerform src False key
	where
		go a = do
			showStart "get" file
			next a	

perform :: Key -> CommandPerform
perform key = stopUnless (getViaTmp key $ getKeyFile key) $
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKeyFile :: Key -> FilePath -> Annex Bool
getKeyFile key file = dispatch =<< Remote.keyPossibilities key
	where
		dispatch [] = do
			showNote "not available"
			Remote.showLocations key []
			return False
		dispatch remotes = trycopy remotes remotes
		trycopy full [] = do
			Remote.showTriedRemotes full
			Remote.showLocations key []
			return False
		trycopy full (r:rs) =
			ifM (probablyPresent r)
				( docopy r (trycopy full rs)
				, trycopy full rs
				)
		-- This check is to avoid an ugly message if a remote is a
		-- drive that is not mounted.
		probablyPresent r
			| Remote.hasKeyCheap r =
				either (const False) id <$> Remote.hasKey r key
			| otherwise = return True
		docopy r continue = do
			showAction $ "from " ++ Remote.name r
			ifM (Remote.retrieveKeyFile r key file)
				( return True , continue)
