{- Standard git remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Git (remote) where

import Control.Exception.Extensible
import Control.Monad.State (liftIO)
import qualified Data.Map as M
import System.Cmd.Utils
import System.Posix.Files

import Types
import Types.Remote
import qualified Git
import qualified Annex
import Locations
import UUID
import Utility
import qualified Content
import Messages
import Utility.CopyFile
import Utility.RsyncFile
import Remote.Ssh
import Config

remote :: RemoteType Annex
remote = RemoteType {
	typename = "git",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

list :: Annex [Git.Repo]
list = do
	g <- Annex.gitRepo
	return $ Git.remotes g

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u _ = do
 	{- It's assumed to be cheap to read the config of non-URL remotes,
	 - so this is done each time git-annex is run. Conversely,
	 - the config of an URL remote is only read when there is no
	 - cached UUID value. -}
	let cheap = not $ Git.repoIsUrl r
	r' <- case (cheap, u) of
		(True, _) -> tryGitConfigRead r
		(False, "") -> tryGitConfigRead r
		_ -> return r

	u' <- getUUID r'

	let defcst = if cheap then cheapRemoteCost else expensiveRemoteCost
	cst <- remoteCost r' defcst

	return Remote {
		uuid = u',
		cost = cst,
		name = Git.repoDescribe r',
		storeKey = copyToRemote r',
		retrieveKeyFile = copyFromRemote r',
		removeKey = dropKey r',
		hasKey = inAnnex r',
		hasKeyCheap = cheap,
		config = Nothing
	}

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| not $ M.null $ Git.configMap r = return r -- already read
	| Git.repoIsSsh r = store $ onRemote r (pipedconfig, r) "configlist" []
	| Git.repoIsUrl r = return r
	| otherwise = store $ safely $ Git.configRead r
	where
		-- Reading config can fail due to IO error or
		-- for other reasons; catch all possible exceptions.
		safely a = do
			result <- liftIO (try a :: IO (Either SomeException Git.Repo))
			case result of
				Left _ -> return r
				Right r' -> return r'
		pipedconfig cmd params = safely $
			pOpen ReadFromPipe cmd (toCommand params) $
				Git.hConfigRead r
		store a = do
			r' <- a
			g <- Annex.gitRepo
			let l = Git.remotes g
			let g' = Git.remotesAdd g $ exchange l r'
			Annex.changeState $ \s -> s { Annex.repo = g' }
			return r'
		exchange [] _ = []
		exchange (old:ls) new =
			if Git.repoRemoteName old == Git.repoRemoteName new
				then new : exchange ls new
				else old : exchange ls new

{- Checks if a given remote has the content for a key inAnnex.
 - If the remote cannot be accessed, returns a Left error.
 -}
inAnnex :: Git.Repo -> Key -> Annex (Either IOException Bool)
inAnnex r key = if Git.repoIsUrl r
		then checkremote
		else liftIO (try checklocal ::IO (Either IOException Bool))
	where
		checklocal = do
			-- run a local check inexpensively,
			-- by making an Annex monad using the remote
			a <- Annex.new r
			Annex.eval a (Content.inAnnex key)
		checkremote = do
			showAction $ "checking " ++ Git.repoDescribe r
			inannex <- onRemote r (boolSystem, False) "inannex" 
				[Param (show key)]
			return $ Right inannex
	
dropKey :: Git.Repo -> Key -> Annex Bool
dropKey r key = 
	onRemote r (boolSystem, False) "dropkey"
		[ Params "--quiet --force"
		, Param $ show key
		]

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> Annex Bool
copyFromRemote r key file
	| not $ Git.repoIsUrl r = rsyncOrCopyFile r (gitAnnexLocation r key) file
	| Git.repoIsSsh r = rsyncHelper =<< rsyncParamsRemote r True key file
	| otherwise = error "copying from non-ssh repo not supported"
		
{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Git.Repo -> Key -> Annex Bool
copyToRemote r key
	| not $ Git.repoIsUrl r = do
		g <- Annex.gitRepo
		let keysrc = gitAnnexLocation g key
		-- run copy from perspective of remote
		liftIO $ do
			a <- Annex.new r
			Annex.eval a $ do
				ok <- Content.getViaTmp key $
					rsyncOrCopyFile r keysrc
				Content.saveState
				return ok
	| Git.repoIsSsh r = do
		g <- Annex.gitRepo
		let keysrc = gitAnnexLocation g key
		rsyncHelper =<< rsyncParamsRemote r False key keysrc
	| otherwise = error "copying to non-ssh repo not supported"

rsyncHelper :: [CommandParam] -> Annex Bool
rsyncHelper p = do
	showOutput -- make way for progress bar
	res <- liftIO $ rsync p
	if res
		then return res
		else do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return res

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: Git.Repo -> FilePath -> FilePath -> Annex Bool
rsyncOrCopyFile r src dest = do
	ss <- liftIO $ getFileStatus $ parentDir src
	ds <- liftIO $ getFileStatus $ parentDir dest
	if deviceID ss == deviceID ds
		then liftIO $ copyFile src dest
		else do
			params <- rsyncParams r
			rsyncHelper $ params ++ [Param src, Param dest]

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Git.Repo -> Bool -> Key -> FilePath -> Annex [CommandParam]
rsyncParamsRemote r sending key file = do
	Just (shellcmd, shellparams) <- git_annex_shell r
		(if sending then "sendkey" else "recvkey")
		[ Param $ show key
		-- Command is terminated with "--", because
		-- rsync will tack on its own options afterwards,
		-- and they need to be ignored.
		, Param "--"
		]
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	o <- rsyncParams r
	if sending
		then return $ o ++ eparam ++ [dummy, File file]
		else return $ o ++ eparam ++ [File file, dummy]
	where
		-- the rsync shell parameter controls where rsync
		-- goes, so the source/dest parameter can be a dummy value,
		-- that just enables remote rsync mode.
		dummy = Param ":"

rsyncParams :: Git.Repo -> Annex [CommandParam]
rsyncParams r = do
	o <- getConfig r "rsync-options" ""
	return $ options ++ map Param (words o)
	where
 		-- --inplace to resume partial files
		options = [Params "-p --progress --inplace"]
