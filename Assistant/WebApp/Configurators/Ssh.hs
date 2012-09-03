{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod
import Assistant.WebApp.Configurators.Local
import qualified Types.Remote as R
import qualified Remote.Rsync as Rsync
import qualified Command.InitRemote
import Logs.UUID
import Logs.Remote

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Network.BSD
import System.Posix.User
import System.Process (CreateProcess(..))

sshConfigurator :: Widget -> Handler RepHtml
sshConfigurator a = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a remote server"
	a

data SshServer = SshServer
	{ hostname :: Maybe Text
	, username :: Maybe Text
	, directory :: Maybe Text
	}
	deriving (Show)

sshServerAForm :: Text -> AForm WebApp WebApp SshServer
sshServerAForm localusername = SshServer
	<$> aopt check_hostname "Host name" Nothing
	<*> aopt check_username "User name" (Just $ Just localusername)
	<*> aopt textField "Directory" (Just $ Just $ T.pack gitAnnexAssistantDefaultDir)
	where
		check_hostname = checkM (liftIO . checkdns) textField
		checkdns t = do
			let h = T.unpack t
			r <- catchMaybeIO $ getHostByName h
			return $ case r of
				-- canonicalize input hostname if it had no dot
				Just hostentry
					| '.' `elem` h -> Right t
					| otherwise -> Right $ T.pack $ hostName hostentry
				Nothing -> Left bad_hostname

		check_username = checkBool (all (`notElem` "/:@ \t") . T.unpack)
			bad_username textField
		
		bad_hostname = "cannot resolve host name" :: Text
		bad_username = "bad user name" :: Text

data ServerStatus
	= UntestedServer
	| UnusableServer Text -- reason why it's not usable
	| UsableRsyncServer
	| UsableSshServer
	deriving (Eq)

usable :: ServerStatus -> Bool
usable UntestedServer = False
usable (UnusableServer _) = False
usable UsableRsyncServer = True
usable UsableSshServer = True

getAddSshR :: Handler RepHtml
getAddSshR = sshConfigurator $ do
	u <- liftIO $ T.pack . userName
		<$> (getUserEntryForID =<< getEffectiveUserID)
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ sshServerAForm u
	case result of
		FormSuccess sshserver -> do
			(status, needspubkey) <- liftIO $ testServer sshserver
			if usable status
				then lift $ redirect $ ConfirmSshR $
					SshData 
						{ sshHostName = fromJust $ hostname sshserver
						, sshUserName = username sshserver
						, sshDirectory = fromMaybe "" $ directory sshserver
						, sshRepoName = genSshRepoName sshserver
						, needsPubKey = needspubkey
						, rsyncOnly = (status == UsableRsyncServer)
						}
				else showform form enctype status
		_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/addssh")

{- Test if we can ssh into the server.
 -
 - Two probe attempts are made. First, try sshing in using the existing
 - configuration, but don't let ssh prompt for any password. If
 - passwordless login is already enabled, use it. Otherwise,
 - a special ssh key will need to be generated just for this server.
 -
 - Once logged into the server, probe to see if git-annex-shell is
 - available, or rsync.
 -}
testServer :: SshServer -> IO (ServerStatus, Bool)
testServer (SshServer { hostname = Nothing }) = return
	(UnusableServer "Please enter a host name.", False)
testServer sshserver = do
	status <- probe sshserver [sshopt "NumberOfPasswordPrompts" "0"]
	if usable status
		then return (status, False)
		else do
			status' <- probe sshserver []
			return (status', True)
	where
		probe s extraopts = do
			knownhost <- knownHost sshserver
			let remotecommand = join ";" $
				[ report "loggedin"
				, checkcommand "git-annex-shell"
				, checkcommand "rsync"
				]
			let sshopts = nonempty $ extraopts ++
				{- If this is an already known host, let
				 - ssh check it as usual.
				 - Otherwise, trust the host key. -}
				[ if knownhost then "" else sshopt "StrictHostKeyChecking" "no"
				, "-n" -- don't read from stdin
				, genSshHost (fromJust $ hostname s) (username s)
				, remotecommand
				]
			parsetranscript . fst <$> sshTranscript sshopts
		parsetranscript s
			| reported "git-annex-shell" = UsableSshServer
			| reported "rsync" = UsableRsyncServer
			| reported "loggedin" = UnusableServer
				"Neither rsync nor git-annex are installed on the server. Perhaps you should go install them?"
			| otherwise = UnusableServer $ T.pack $
				"Failed to ssh to the server. Transcript: " ++ s
			where
				reported r = token r `isInfixOf` s
		nonempty = filter $ not . null
		checkcommand c = "if which " ++ c ++ "; then " ++ report c ++ "; fi"
		token r = "git-annex-probe " ++ r
		report r = "echo " ++ token r
		sshopt k v = concat ["-o", k, "=", v]

sshDir :: IO FilePath
sshDir = do
	home <- myHomeDir
	return $ home </> ".ssh"

{- user@host or host -}
genSshHost :: Text -> Maybe Text -> String
genSshHost host user = maybe "" (\v -> T.unpack v ++ "@") user ++ T.unpack host

{- host_dir -}
genSshRepoName :: SshServer -> String
genSshRepoName s = (T.unpack $ fromJust $ hostname s) ++
	(maybe "" (\d -> '_' : T.unpack d) (directory s))

{- The output of ssh, including both stdout and stderr. -}
sshTranscript :: [String] -> IO (String, Bool)
sshTranscript opts = do
	(readf, writef) <- createPipe
	readh <- fdToHandle readf
	writeh <- fdToHandle writef
	(_, _, _, pid) <- createProcess $
		(proc "ssh" opts)
			{ std_in = Inherit
			, std_out = UseHandle writeh
			, std_err = UseHandle writeh
			}
	hClose writeh
	transcript <- hGetContentsStrict readh
	hClose readh
	ok <- checkSuccessProcess pid
	return (transcript, ok)

{- Does ssh have known_hosts data for a hostname? -}
knownHost :: SshServer -> IO Bool
knownHost (SshServer { hostname = Nothing }) = return False
knownHost (SshServer { hostname = Just h }) = do
	sshdir <- sshDir
	ifM (doesFileExist $ sshdir </> "known_hosts")
		( not . null <$> readProcess "ssh-keygen" ["-F", T.unpack h]
		, return False
		)

getConfirmSshR :: SshData -> Handler RepHtml
getConfirmSshR sshdata = sshConfigurator $ do
	let authtoken = webAppFormAuthToken
	$(widgetFile "configurators/confirmssh")

getMakeSshGitR :: SshData -> Handler RepHtml
getMakeSshGitR = makeSsh False

getMakeSshRsyncR :: SshData -> Handler RepHtml
getMakeSshRsyncR = makeSsh True

makeSsh :: Bool -> SshData -> Handler RepHtml
makeSsh rsync sshdata
	| needsPubKey sshdata = do
		(pubkey, sshdata') <- liftIO $ genSshKey sshdata
		makeSsh' rsync sshdata' (Just pubkey)
	| otherwise = makeSsh' rsync sshdata Nothing

makeSsh' :: Bool -> SshData -> Maybe String -> Handler RepHtml
makeSsh' rsync sshdata pubkey = do
	(transcript, ok) <- liftIO $ sshTranscript [sshhost, remoteCommand]
	if ok
		then do
			r <- runAnnex undefined makerepo
			syncRemote r
			redirect RepositoriesR
		else showerr transcript
	where
		sshhost = genSshHost (sshHostName sshdata) (sshUserName sshdata)
		remotedir = T.unpack $ sshDirectory sshdata
		remoteCommand = join "&&" $ catMaybes
			[ Just $ "mkdir -p " ++ shellEscape remotedir
			, Just $ "cd " ++ shellEscape remotedir
			, if rsync then Nothing else Just $ "git init --bare --shared"
			, if rsync then Nothing else Just $ "git annex init"
			, maybe Nothing (makeAuthorizedKeys sshdata) pubkey
			]
		showerr msg = sshConfigurator $
			$(widgetFile "configurators/makessherror")
		{- This is a one-sided remote setup; the remote server
		 - probably does not have a route to the client here. -}
		makerepo = addRemote $
			(if rsync then makeRsyncRemote else makeGitRemote)
				(sshRepoName sshdata) sshurl
		sshurl = T.unpack $ T.concat $ if rsync
			then [u, h, ":", sshDirectory sshdata, "/"]
			else ["ssh://", u, h, d, "/"]
			where
				u = maybe "" (\v -> T.concat [v, "@"]) $ sshUserName sshdata
				h = sshHostName sshdata
				d
					| "/" `T.isPrefixOf` sshDirectory sshdata = d
					| otherwise = T.concat ["/~/", sshDirectory sshdata]

{- Inits a rsync special remote, and returns the name of the remote. -}
makeRsyncRemote :: String -> String -> Annex String
makeRsyncRemote name location = makeRemote name location $ const $ do
	(u, c) <- Command.InitRemote.findByName name
	c' <- R.setup Rsync.remote u $ M.union config c
	describeUUID u name
	configSet u c'
	where
		config = M.fromList
			[ ("encryption", "shared")
			, ("rsyncurl", location)
			, ("type", "rsync")
			]

makeAuthorizedKeys :: SshData -> String -> Maybe String
makeAuthorizedKeys sshdata pubkey
	| needsPubKey sshdata = Just $ join "&&" $
		[ "mkdir -p ~/.ssh"
		, "touch ~/.ssh/authorized_keys"
		, "chmod 600 ~/.ssh/authorized_keys"
		, unwords
			[ "echo"
			, shellEscape $ authorizedKeysLine sshdata pubkey
			, ">>~/.ssh/authorized_keys"
			]
		]
	| otherwise = Nothing
		
authorizedKeysLine :: SshData -> String -> String
authorizedKeysLine sshdata pubkey 
	{- TODO: Locking down rsync is difficult, requiring a rather
	 - long perl script. -}
	| rsyncOnly sshdata = pubkey
	| otherwise = limitcommand "git-annex-shell -c" ++ pubkey
	where
		limitcommand c = "command=\"perl -e 'exec qw(" ++ c ++ "), $ENV{SSH_ORIGINAL_COMMAND}'\",no-agent-forwarding,no-port-forwarding,no-X11-forwarding "

{- Returns the public key content, and a modified SshData with a
 - mangled hostname that will enable use of the key.
 - This way we avoid changing the user's regular ssh experience at all. -}
genSshKey :: SshData -> IO (String, SshData)
genSshKey sshdata = do
	sshdir <- sshDir
	let configfile = sshdir </> "config"
	createDirectoryIfMissing True sshdir
	unlessM (doesFileExist $ sshdir </> sshprivkeyfile) $ do
		ok <- boolSystem "ssh-keygen"
			[ Param "-P", Param "" -- no password
			, Param "-f", File $ sshdir </> sshprivkeyfile
			]
		unless ok $
			error "ssh-keygen failed"
	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $
		appendFile configfile $ unlines
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			, "\tHostname " ++ T.unpack (sshHostName sshdata)
			, "\tIdentityFile ~/.ssh/" ++ sshprivkeyfile
			]
	pubkey <- readFile $ sshdir </> sshpubkeyfile
	return (pubkey, sshdata { sshHostName = T.pack mangledhost })
	where
		sshprivkeyfile = "key." ++ mangledhost
		sshpubkeyfile = sshprivkeyfile ++ ".pub"
		mangledhost = "git-annex-" ++ T.unpack (sshHostName sshdata) ++ user
		user = maybe "" (\u -> "-" ++ T.unpack u) (sshUserName sshdata)
