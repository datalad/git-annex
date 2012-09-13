{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.Common
import Assistant.Ssh
import Assistant.MakeRemote
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Network.BSD
import System.Posix.User

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

{- SshServer is only used for applicative form prompting, this converts
 - the result of such a form into a SshData. -}
mkSshData :: SshServer -> SshData
mkSshData sshserver = SshData 
	{ sshHostName = fromMaybe "" $ hostname sshserver
	, sshUserName = username sshserver
	, sshDirectory = fromMaybe "" $ directory sshserver
	, sshRepoName = genSshRepoName
		(T.unpack $ fromJust $ hostname sshserver)
		(maybe "" T.unpack $ directory sshserver)
	, needsPubKey = False
	, rsyncOnly = False
	}

sshServerAForm :: Maybe Text -> AForm WebApp WebApp SshServer
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
						, rsyncOnly = status == UsableRsyncServer
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
testServer sshserver@(SshServer { hostname = Just hn }) = do
	status <- probe [sshOpt "NumberOfPasswordPrompts" "0"]
	if usable status
		then return (status, False)
		else do
			status' <- probe []
			return (status', True)
	where
		probe extraopts = do
			let remotecommand = join ";"
				[ report "loggedin"
				, checkcommand "git-annex-shell"
				, checkcommand "rsync"
				]
			knownhost <- knownHost hn
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
		keypair <- liftIO genSshKeyPair
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
			, if rsync then Nothing else Just "git init --bare --shared"
			, if rsync then Nothing else Just "git annex init"
			, if needsPubKey sshdata
				then addAuthorizedKeysCommand (rsyncOnly sshdata) . sshPubKey <$> keypair
				else Nothing
			]

makeSshRepo :: Bool -> SshData -> Handler RepHtml
makeSshRepo forcersync sshdata = do
	webapp <- getYesod
	liftIO $ makeSshRemote
		(fromJust $ threadState webapp)
		(daemonStatus webapp)
		(scanRemotes webapp)
		forcersync sshdata
	redirect RepositoriesR

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
			knownhost <- liftIO $ maybe (return False) knownHost (hostname sshserver)
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
			let remotecommand = join ";"
				[ "mkdir -p .ssh"
				, "touch .ssh/authorized_keys"
				, "dd of=.ssh/authorized_keys oflag=append conv=notrunc"
				, "mkdir -p " ++ T.unpack (sshDirectory sshdata)
				]
			let sshopts = filter (not . null)
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
