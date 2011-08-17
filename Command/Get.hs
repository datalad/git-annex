{- git-annex command
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Command
import qualified Annex
import qualified Remote
import Types
import Content
import Messages
import qualified Command.Move

command :: [Command]
command = [repoCommand "get" paramPath seek
		"make content of annexed files available"]

seek :: [CommandSeek]
seek = [withFilesInGit start]

start :: CommandStartString
start file = isAnnexed file $ \(key, _) -> do
	inannex <- inAnnex key
	if inannex
		then stop
		else do
			showStart "get" file
			from <- Annex.getState Annex.fromremote
			case from of
				Nothing -> next $ perform key
				Just name -> do
					src <- Remote.byName name
					next $ Command.Move.fromPerform src False key

perform :: Key -> CommandPerform
perform key = do
	ok <- getViaTmp key (getKeyFile key)
	if ok
		then next $ return True -- no cleanup needed
		else stop

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
