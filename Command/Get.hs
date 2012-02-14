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
	autoCopies file key (<) $ \_numcopies -> do
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
perform key = stopUnless (getViaTmp key $ getKeyFile key) $ do
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKeyFile :: Key -> FilePath -> Annex Bool
getKeyFile key file = do
	remotes <- Remote.keyPossibilities key
	if null remotes
		then do
			showNote "not available"
			Remote.showLocations key []
			return False
		else trycopy remotes remotes
	where
		trycopy full [] = do
			Remote.showTriedRemotes full
			Remote.showLocations key []
			return False
		trycopy full (r:rs) = do
			probablythere <- probablyPresent r
			if probablythere
				then docopy r (trycopy full rs)
				else trycopy full rs
		-- This check is to avoid an ugly message if a remote is a
		-- drive that is not mounted.
		probablyPresent r =
			if Remote.hasKeyCheap r
				then do
					res <- Remote.hasKey r key
					case res of
						Right b -> return b
						Left _ -> return False
				else return True
		docopy r continue = do
			showAction $ "from " ++ Remote.name r
			copied <- Remote.retrieveKeyFile r key file
			if copied
				then return True
				else continue
