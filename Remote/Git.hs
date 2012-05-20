{- Standard git remotes.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Git (remote, repoAvail) where

import qualified Data.Map as M
import Control.Exception.Extensible

import Common.Annex
import Utility.CopyFile
import Utility.RsyncFile
import Remote.Helper.Ssh
import Types.Remote
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.Construct
import qualified Annex
import Logs.Presence
import Annex.UUID
import qualified Annex.Content
import qualified Annex.BranchState
import qualified Annex.Branch
import qualified Utility.Url as Url
import Utility.TempFile
import Config
import Init
import Types.Key

remote :: RemoteType
remote = RemoteType {
	typename = "git",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

list :: Annex [Git.Repo]
list = do
	c <- fromRepo Git.config
	mapM (tweakurl c) =<< fromRepo Git.remotes
	where
		annexurl n = "remote." ++ n ++ ".annexurl"
		tweakurl c r = do
			let n = fromJust $ Git.remoteName r
			case M.lookup (annexurl n) c of
				Nothing -> return r
				Just url -> inRepo $ \g ->
					Git.Construct.remoteNamed n $
						Git.Construct.fromRemoteLocation url g

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u _ = do
 	{- It's assumed to be cheap to read the config of non-URL remotes,
	 - so this is done each time git-annex is run. Conversely,
	 - the config of an URL remote is only read when there is no
	 - cached UUID value. -}
	let cheap = not $ Git.repoIsUrl r
	notignored <- repoNotIgnored r
	r' <- case (cheap, notignored, u) of
		(_, False, _) -> return r
		(True, _, _) -> tryGitConfigRead r
		(False, _, NoUUID) -> tryGitConfigRead r
		_ -> return r

	u' <- getRepoUUID r'

	let defcst = if cheap then cheapRemoteCost else expensiveRemoteCost
	cst <- remoteCost r' defcst

	return Remote {
		uuid = u',
		cost = cst,
		name = Git.repoDescribe r',
		storeKey = copyToRemote r',
		retrieveKeyFile = copyFromRemote r',
		retrieveKeyFileCheap = copyFromRemoteCheap r',
		removeKey = dropKey r',
		hasKey = inAnnex r',
		hasKeyCheap = cheap,
		whereisKey = Nothing,
		config = Nothing,
		repo = r',
		remotetype = remote
	}

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Git.Repo -> Annex Git.Repo
tryGitConfigRead r 
	| not $ M.null $ Git.config r = return r -- already read
	| Git.repoIsSsh r = store $ onRemote r (pipedconfig, r) "configlist" []
	| Git.repoIsHttp r = do
		headers <- getHttpHeaders
		store $ safely $ geturlconfig headers
	| Git.repoIsUrl r = return r
	| otherwise = store $ safely $ onLocal r $ do 
		ensureInitialized
		Annex.getState Annex.repo
	where
		-- Reading config can fail due to IO error or
		-- for other reasons; catch all possible exceptions.
		safely a = either (const $ return r) return
				=<< liftIO (try a :: IO (Either SomeException Git.Repo))

		pipedconfig cmd params = safely $
			pOpen ReadFromPipe cmd (toCommand params) $
				Git.Config.hRead r

		geturlconfig headers = do
			s <- Url.get (Git.repoLocation r ++ "/config") headers
			withTempFile "git-annex.tmp" $ \tmpfile h -> do
				hPutStr h s
				hClose h
				pOpen ReadFromPipe "git" ["config", "--null", "--list", "--file", tmpfile] $
					Git.Config.hRead r

		store = observe $ \r' -> do
			g <- gitRepo
			let l = Git.remotes g
			let g' = g { Git.remotes = exchange l r' }
			Annex.changeState $ \s -> s { Annex.repo = g' }

		exchange [] _ = []
		exchange (old:ls) new
			| Git.remoteName old == Git.remoteName new =
				new : exchange ls new
			| otherwise =
				old : exchange ls new

{- Checks if a given remote has the content for a key inAnnex.
 - If the remote cannot be accessed, or if it cannot determine
 - whether it has the content, returns a Left error message.
 -}
inAnnex :: Git.Repo -> Key -> Annex (Either String Bool)
inAnnex r key
	| Git.repoIsHttp r = checkhttp =<< getHttpHeaders
	| Git.repoIsUrl r = checkremote
	| otherwise = checklocal
	where
		checkhttp headers = liftIO $ go undefined $ keyUrls r key
			where
				go e [] = return $ Left e
				go _ (u:us) = do
					res <- catchMsgIO $
						Url.check u headers (keySize key)
					case res of
						Left e -> go e us
						v -> return v
		checkremote = do
			showAction $ "checking " ++ Git.repoDescribe r
			onRemote r (check, unknown) "inannex" [Param (show key)]
			where
				check c p = dispatch <$> safeSystem c p
				dispatch ExitSuccess = Right True
				dispatch (ExitFailure 1) = Right False
				dispatch _ = unknown
		checklocal = dispatch <$> check
			where
				check = liftIO $ catchMsgIO $ onLocal r $
					Annex.Content.inAnnexSafe key
				dispatch (Left e) = Left e
				dispatch (Right (Just b)) = Right b
				dispatch (Right Nothing) = unknown
		unknown = Left $ "unable to check " ++ Git.repoDescribe r

{- Checks inexpensively if a repository is available for use. -}
repoAvail :: Git.Repo -> Annex Bool
repoAvail r 
	| Git.repoIsHttp r = return True
	| Git.repoIsUrl r = return True
	| otherwise = liftIO $ catchBoolIO $ onLocal r $ return True

{- Runs an action on a local repository inexpensively, by making an annex
 - monad using that repository. -}
onLocal :: Git.Repo -> Annex a -> IO a
onLocal r a = do
	s <- Annex.new r
	Annex.eval s $ do
		-- No need to update the branch; its data is not used
		-- for anything onLocal is used to do.
		Annex.BranchState.disableUpdate
		liftIO Git.Command.reap `after` a

keyUrls :: Git.Repo -> Key -> [String]
keyUrls r key = map tourl (annexLocations key)
	where
		tourl l = Git.repoLocation r ++ "/" ++ l

dropKey :: Git.Repo -> Key -> Annex Bool
dropKey r key
	| not $ Git.repoIsUrl r = commitOnCleanup r $ liftIO $ onLocal r $ do
		ensureInitialized
		whenM (Annex.Content.inAnnex key) $ do
			Annex.Content.lockContent key $
				Annex.Content.removeAnnex key
			Annex.Content.logStatus key InfoMissing
			Annex.Content.saveState True
		return True
	| Git.repoIsHttp r = error "dropping from http repo not supported"
	| otherwise = commitOnCleanup r $ onRemote r (boolSystem, False) "dropkey"
		[ Params "--quiet --force"
		, Param $ show key
		]

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Git.Repo -> Key -> FilePath -> Annex Bool
copyFromRemote r key file
	| not $ Git.repoIsUrl r = do
		params <- rsyncParams r
		loc <- liftIO $ gitAnnexLocation key r
		rsyncOrCopyFile params loc file
	| Git.repoIsSsh r = rsyncHelper =<< rsyncParamsRemote r True key file
	| Git.repoIsHttp r = Annex.Content.downloadUrl (keyUrls r key) file
	| otherwise = error "copying from non-ssh, non-http repo not supported"

copyFromRemoteCheap :: Git.Repo -> Key -> FilePath -> Annex Bool
copyFromRemoteCheap r key file
	| not $ Git.repoIsUrl r = do
		loc <- liftIO $ gitAnnexLocation key r
		liftIO $ catchBoolIO $ createSymbolicLink loc file >> return True
	| Git.repoIsSsh r =
		ifM (Annex.Content.preseedTmp key file)
			( copyFromRemote r key file
			, return False
			)
	| otherwise = return False

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Git.Repo -> Key -> Annex Bool
copyToRemote r key
	| not $ Git.repoIsUrl r = commitOnCleanup r $ do
		keysrc <- inRepo $ gitAnnexLocation key
		params <- rsyncParams r
		-- run copy from perspective of remote
		liftIO $ onLocal r $ do
			ensureInitialized
			Annex.Content.saveState True `after`
				Annex.Content.getViaTmp key
					(rsyncOrCopyFile params keysrc)
	| Git.repoIsSsh r = commitOnCleanup r $ do
		keysrc <- inRepo $ gitAnnexLocation key
		rsyncHelper =<< rsyncParamsRemote r False key keysrc
	| otherwise = error "copying to non-ssh repo not supported"

rsyncHelper :: [CommandParam] -> Annex Bool
rsyncHelper p = do
	showOutput -- make way for progress bar
	ifM (liftIO $ rsync p)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: [CommandParam] -> FilePath -> FilePath -> Annex Bool
rsyncOrCopyFile rsyncparams src dest =
	ifM (sameDeviceIds src dest)
		( liftIO $ copyFileExternal src dest
		, rsyncHelper $ rsyncparams ++ [Param src, Param dest]
		)
	where
		sameDeviceIds a b = (==) <$> (getDeviceId a) <*> (getDeviceId b)
		getDeviceId f = deviceID <$> liftIO (getFileStatus $ parentDir f)

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
		-- The rsync shell parameter controls where rsync
		-- goes, so the source/dest parameter can be a dummy value,
		-- that just enables remote rsync mode.
		-- For maximum compatability with some patched rsyncs,
		-- the dummy value needs to still contain a hostname,
		-- even though this hostname will never be used.
		dummy = Param "dummy:"

rsyncParams :: Git.Repo -> Annex [CommandParam]
rsyncParams r = do
	o <- getRemoteConfig r "rsync-options" ""
	return $ options ++ map Param (words o)
	where
 		-- --inplace to resume partial files
		options = [Params "-p --progress --inplace"]

commitOnCleanup :: Git.Repo -> Annex a -> Annex a
commitOnCleanup r a = go `after` a
	where
		go = Annex.addCleanup (Git.repoLocation r) cleanup
		cleanup
			| not $ Git.repoIsUrl r = liftIO $ onLocal r $
				doQuietSideAction $
					Annex.Branch.commit "update"
			| otherwise = void $ do
				Just (shellcmd, shellparams) <-
					git_annex_shell r "commit" []
				-- Throw away stderr, since the remote may not
				-- have a new enough git-annex shell to
				-- support committing.
				let cmd = shellcmd ++ " "
					++ unwords (map shellEscape $ toCommand shellparams)
					++ ">/dev/null 2>/dev/null"
				liftIO $ boolSystem "sh" [Param "-c", Param cmd]
