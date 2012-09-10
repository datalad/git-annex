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
import Utility.TempFile
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
import qualified Control.Exception as E
import Network.BSD
import System.Posix.User
import System.Process (CreateProcess(..))
import Control.Concurrent

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

data SshKeyPair = SshKeyPair
	{ sshPubKey :: String
	, sshPrivKey :: String
	}

{- SshServer is only used for applicative form prompting, this converts
 - the result of such a form into a SshData. -}
mkSshData :: SshServer -> SshData
mkSshData sshserver = SshData 
	{ sshHostName = fromMaybe "" $ hostname sshserver
	, sshUserName = username sshserver
	, sshDirectory = fromMaybe "" $ directory sshserver
	, sshRepoName = genSshRepoName sshserver
	, needsPubKey = False
	, rsyncOnly = False
	}

sshServerAForm :: (Maybe Text) -> AForm WebApp WebApp SshServer
sshServerAForm localusername = SshServer
	<$> aopt check_hostname "Host name" Nothing
	<*> aopt check_username "User name" (Just localusername)
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
		runFormGet $ renderBootstrap $ sshServerAForm (Just u)
	case result of
		FormSuccess sshserver -> do
			(status, needspubkey) <- liftIO $ testServer sshserver
			if usable status
				then lift $ redirect $ ConfirmSshR $
					(mkSshData sshserver)
						{ needsPubKey = needspubkey
						, rsyncOnly = (status == UsableRsyncServer)
						}
				else showform form enctype status
		_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/ssh/add")

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
	status <- probe [sshOpt "NumberOfPasswordPrompts" "0"]
	if usable status
		then return (status, False)
		else do
			status' <- probe []
			return (status', True)
	where
		probe extraopts = do
			let remotecommand = join ";" $
				[ report "loggedin"
				, checkcommand "git-annex-shell"
				, checkcommand "rsync"
				]
			knownhost <- knownHost sshserver
			let sshopts = filter (not . null) $ extraopts ++
				{- If this is an already known host, let
				 - ssh check it as usual.
				 - Otherwise, trust the host key. -}
				[ if knownhost then "" else sshOpt "StrictHostKeyChecking" "no"
				, "-n" -- don't read from stdin
				, genSshHost (fromJust $ hostname sshserver) (username sshserver)
				, remotecommand
				]
			parsetranscript . fst <$> sshTranscript sshopts ""
		parsetranscript s
			| reported "git-annex-shell" = UsableSshServer
			| reported "rsync" = UsableRsyncServer
			| reported "loggedin" = UnusableServer
				"Neither rsync nor git-annex are installed on the server. Perhaps you should go install them?"
			| otherwise = UnusableServer $ T.pack $
				"Failed to ssh to the server. Transcript: " ++ s
			where
				reported r = token r `isInfixOf` s
		checkcommand c = "if which " ++ c ++ "; then " ++ report c ++ "; fi"
		token r = "git-annex-probe " ++ r
		report r = "echo " ++ token r

{- ssh -ofoo=bar command-line option -}
sshOpt :: String -> String -> String
sshOpt k v = concat ["-o", k, "=", v]

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
sshTranscript :: [String] -> String -> IO (String, Bool)
sshTranscript opts input = do
	(readf, writef) <- createPipe
	readh <- fdToHandle readf
	writeh <- fdToHandle writef
	(Just inh, _, _, pid) <- createProcess $
		(proc "ssh" opts)
			{ std_in = CreatePipe
			, std_out = UseHandle writeh
			, std_err = UseHandle writeh
			}
	hClose writeh

	-- fork off a thread to start consuming the output
	transcript <- hGetContents readh
	outMVar <- newEmptyMVar
	_ <- forkIO $ E.evaluate (length transcript) >> putMVar outMVar ()

	-- now write and flush any input
	when (not (null input)) $ do hPutStr inh input; hFlush inh
	hClose inh -- done with stdin

	-- wait on the output
	takeMVar outMVar
	hClose readh

	ok <- checkSuccessProcess pid
	return ()
	return (transcript, ok)

{- Runs a ssh command; if it fails shows the user the transcript,
 - and if it succeeds, runs an action. -}
sshSetup :: [String] -> String -> Handler RepHtml -> Handler RepHtml
sshSetup opts input a = do
	(transcript, ok) <- liftIO $ sshTranscript opts input
	if ok
		then a
		else showSshErr transcript

showSshErr :: String -> Handler RepHtml
showSshErr msg = sshConfigurator $
	$(widgetFile "configurators/ssh/error")

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
	$(widgetFile "configurators/ssh/confirm")

getMakeSshGitR :: SshData -> Handler RepHtml
getMakeSshGitR = makeSsh False

getMakeSshRsyncR :: SshData -> Handler RepHtml
getMakeSshRsyncR = makeSsh True

makeSsh :: Bool -> SshData -> Handler RepHtml
makeSsh rsync sshdata
	| needsPubKey sshdata = do
		keypair <- liftIO $ genSshKeyPair
		sshdata' <- liftIO $ setupSshKeyPair keypair sshdata
		makeSsh' rsync sshdata' (Just keypair)
	| otherwise = makeSsh' rsync sshdata Nothing

makeSsh' :: Bool -> SshData -> Maybe SshKeyPair -> Handler RepHtml
makeSsh' rsync sshdata keypair =
	sshSetup [sshhost, remoteCommand] "" $
		makeSshRepo rsync sshdata
	where
		sshhost = genSshHost (sshHostName sshdata) (sshUserName sshdata)
		remotedir = T.unpack $ sshDirectory sshdata
		remoteCommand = join "&&" $ catMaybes
			[ Just $ "mkdir -p " ++ shellEscape remotedir
			, Just $ "cd " ++ shellEscape remotedir
			, if rsync then Nothing else Just $ "git init --bare --shared"
			, if rsync then Nothing else Just $ "git annex init"
			, maybe Nothing (makeAuthorizedKeys sshdata) keypair
			]

makeSshRepo :: Bool -> SshData -> Handler RepHtml
makeSshRepo forcersync sshdata = do
	r <- runAnnex undefined $
		addRemote $ maker (sshRepoName sshdata) sshurl
	syncRemote r
	redirect RepositoriesR
	where
		rsync = forcersync || rsyncOnly sshdata
		maker
			| rsync = makeRsyncRemote
			| otherwise = makeGitRemote
		sshurl = T.unpack $ T.concat $
			if rsync
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

makeAuthorizedKeys :: SshData -> SshKeyPair -> Maybe String
makeAuthorizedKeys sshdata keypair
	| needsPubKey sshdata = Just $ join "&&" $
		[ "mkdir -p ~/.ssh"
		, "touch ~/.ssh/authorized_keys"
		, "chmod 600 ~/.ssh/authorized_keys"
		, unwords
			[ "echo"
			, shellEscape $ authorizedKeysLine sshdata keypair
			, ">>~/.ssh/authorized_keys"
			]
		]
	| otherwise = Nothing
		
authorizedKeysLine :: SshData -> SshKeyPair -> String
authorizedKeysLine sshdata (SshKeyPair { sshPubKey = pubkey })
	{- TODO: Locking down rsync is difficult, requiring a rather
	 - long perl script. -}
	| rsyncOnly sshdata = pubkey
	| otherwise = limitcommand "git-annex-shell -c" ++ pubkey
	where
		limitcommand c = "command=\"perl -e 'exec qw(" ++ c ++ "), $ENV{SSH_ORIGINAL_COMMAND}'\",no-agent-forwarding,no-port-forwarding,no-X11-forwarding "

{- Generates a ssh key pair. -}
genSshKeyPair :: IO SshKeyPair
genSshKeyPair = withTempDir "git-annex-keygen" $ \dir -> do
	ok <- boolSystem "ssh-keygen"
		[ Param "-P", Param "" -- no password
		, Param "-f", File $ dir </> "key"
		]
	unless ok $
		error "ssh-keygen failed"
	SshKeyPair
		<$> readFile (dir </> "key.pub")
		<*> readFile (dir </> "key")

{- Installs a ssh key pair, and sets up ssh config with a mangled hostname
 - that will enable use of the key. This way we avoid changing the user's
 - regular ssh experience at all. Returns a modified SshData containing the
 - mangled hostname. -}
setupSshKeyPair :: SshKeyPair -> SshData -> IO SshData
setupSshKeyPair sshkeypair sshdata = do
	sshdir <- sshDir
	let configfile = sshdir </> "config"
	createDirectoryIfMissing True sshdir

	unlessM (doesFileExist $ sshdir </> sshprivkeyfile) $ do
		h <- fdToHandle =<<
			createFile (sshdir </> sshprivkeyfile)
				(unionFileModes ownerWriteMode ownerReadMode)
		hPutStr h (sshPrivKey sshkeypair)
		hClose h
	unlessM (doesFileExist $ sshdir </> sshpubkeyfile) $ do
		writeFile (sshdir </> sshpubkeyfile) (sshPubKey sshkeypair)

	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $
		appendFile configfile $ unlines
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			, "\tHostname " ++ T.unpack (sshHostName sshdata)
			, "\tIdentityFile ~/.ssh/" ++ sshprivkeyfile
			]

	return $ sshdata { sshHostName = T.pack mangledhost }
	where
		sshprivkeyfile = "key." ++ mangledhost
		sshpubkeyfile = sshprivkeyfile ++ ".pub"
		mangledhost = "git-annex-" ++ T.unpack (sshHostName sshdata) ++ user
		user = maybe "" (\u -> "-" ++ T.unpack u) (sshUserName sshdata)

getAddRsyncNetR :: Handler RepHtml
getAddRsyncNetR = do
	((result, form), enctype) <- runFormGet $
		renderBootstrap $ sshServerAForm Nothing
	let showform status = bootstrap (Just Config) $ do
		sideBarDisplay
		setTitle "Add a Rsync.net repository"	
		let authtoken = webAppFormAuthToken
		$(widgetFile "configurators/addrsync.net")
	case result of
		FormSuccess sshserver -> do
			knownhost <- liftIO $ knownHost sshserver
			keypair <- liftIO $ genSshKeyPair
			sshdata <- liftIO $ setupSshKeyPair keypair
				(mkSshData sshserver)
					{ needsPubKey = True
					, rsyncOnly = True
					, sshRepoName = "rsync.net"
					}
			{- I'd prefer to separate commands with && , but
			 - rsync.net's shell does not support that.
			 -
			 - The dd method of appending to the
			 - authorized_keys file is the one recommended by
			 - rsync.net documentation. I touch the file first
			 - to not need to use a different method to create
			 - it.
			 -}
			let remotecommand = join ";" $
				[ "mkdir -p .ssh"
				, "touch .ssh/authorized_keys"
				, "dd of=.ssh/authorized_keys oflag=append conv=notrunc"
				, "mkdir -p " ++ T.unpack (sshDirectory sshdata)
				]
			let sshopts = filter (not . null) $
				[ if knownhost then "" else sshOpt "StrictHostKeyChecking" "no"
				, genSshHost (sshHostName sshdata) (sshUserName sshdata)
				, remotecommand
				]

			let host = fromMaybe "" $ hostname sshserver
			checkhost host showform $
				sshSetup sshopts (sshPubKey keypair) $
					makeSshRepo True sshdata
		_ -> showform UntestedServer
	where
		checkhost host showform a
			| ".rsync.net" `T.isSuffixOf` T.toLower host = a
			| otherwise = showform $ UnusableServer
				"That is not a rsync.net host name."
