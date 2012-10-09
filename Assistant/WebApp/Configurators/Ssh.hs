{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Utility.Rsync (rsyncUrlIsShell)
import Logs.Remote
import Remote
import Logs.Group

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S
import Network.Socket
import System.Posix.User

sshConfigurator :: Widget -> Handler RepHtml
sshConfigurator a = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a remote server"
	a

data SshInput = SshInput
	{ hostname :: Maybe Text
	, username :: Maybe Text
	, directory :: Maybe Text
	}
	deriving (Show)

{- SshInput is only used for applicative form prompting, this converts
 - the result of such a form into a SshData. -}
mkSshData :: SshInput -> SshData
mkSshData s = SshData 
	{ sshHostName = fromMaybe "" $ hostname s
	, sshUserName = username s
	, sshDirectory = fromMaybe "" $ directory s
	, sshRepoName = genSshRepoName
		(T.unpack $ fromJust $ hostname s)
		(maybe "" T.unpack $ directory s)
	, needsPubKey = False
	, rsyncOnly = False
	}

sshInputAForm :: SshInput -> AForm WebApp WebApp SshInput
sshInputAForm def = SshInput
	<$> aopt check_hostname "Host name" (Just $ hostname def)
	<*> aopt check_username "User name" (Just $ username def)
	<*> aopt textField "Directory" (Just $ Just $ fromMaybe (T.pack gitAnnexAssistantDefaultDir) $ directory def)
	where
		check_hostname = checkM (liftIO . checkdns) textField
		checkdns t = do
			let h = T.unpack t
			r <- catchMaybeIO $ getAddrInfo canonname (Just h) Nothing
			return $ case catMaybes . map addrCanonName <$> r of
				-- canonicalize input hostname if it had no dot
				Just (fullname:_)
					| '.' `elem` h -> Right t
					| otherwise -> Right $ T.pack fullname
				Just [] -> Right t
				Nothing -> Left bad_hostname
		canonname = Just $ defaultHints { addrFlags = [AI_CANONNAME] }

		check_username = checkBool (all (`notElem` "/:@ \t") . T.unpack)
			bad_username textField
		
		bad_hostname = "cannot resolve host name" :: Text
		bad_username = "bad user name" :: Text

data ServerStatus
	= UntestedServer
	| UnusableServer Text -- reason why it's not usable
	| UsableRsyncServer
	| UsableSshInput
	deriving (Eq)

usable :: ServerStatus -> Bool
usable UntestedServer = False
usable (UnusableServer _) = False
usable UsableRsyncServer = True
usable UsableSshInput = True

getAddSshR :: Handler RepHtml
getAddSshR = sshConfigurator $ do
	u <- liftIO $ T.pack . userName
		<$> (getUserEntryForID =<< getEffectiveUserID)
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ sshInputAForm $
			SshInput Nothing (Just u) Nothing
	case result of
		FormSuccess sshinput -> do
			s <- liftIO $ testServer sshinput
			case s of
				Left status -> showform form enctype status
				Right sshdata -> lift $ redirect $ ConfirmSshR sshdata
		_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/ssh/add")

{- To enable an existing rsync special remote, parse the SshInput from
 - its rsyncurl, and display a form whose only real purpose is to check
 - if ssh public keys need to be set up. From there, we can proceed with
 - the usual repo setup; all that code is idempotent.
 -
 - Note that there's no EnableSshR because ssh remotes are not special
 - remotes, and so their configuration is not shared between repositories.
 -}
getEnableRsyncR :: UUID -> Handler RepHtml
getEnableRsyncR u = do
	m <- runAnnex M.empty readRemoteLog
	case parseSshRsyncUrl =<< M.lookup "rsyncurl" =<< M.lookup u m of
		Nothing -> redirect AddSshR
		Just sshinput -> sshConfigurator $ do
			((result, form), enctype) <- lift $
				runFormGet $ renderBootstrap $ sshInputAForm sshinput
			case result of
				FormSuccess sshinput'
					| isRsyncNet (hostname sshinput') ->
						void $ lift $ makeRsyncNet sshinput' (const noop)
					| otherwise -> do
						s <- liftIO $ testServer sshinput'
						case s of
							Left status -> showform form enctype status
							Right sshdata -> enable sshdata
				_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			description <- lift $ runAnnex "" $
				T.pack . concat <$> prettyListUUIDs [u]
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/ssh/enable")
		enable sshdata = 
			lift $ redirect $ ConfirmSshR $
				sshdata { rsyncOnly = True }

{- Converts a rsyncurl value to a SshInput. But only if it's a ssh rsync
 - url; rsync:// urls or bare path names are not supported.
 -
 - The hostname is stored mangled in the remote log for rsync special
 - remotes configured by this webapp. So that mangling has to reversed
 - here to get back the original hostname.
 -}
parseSshRsyncUrl :: String -> Maybe SshInput
parseSshRsyncUrl u
	| not (rsyncUrlIsShell u) = Nothing
	| otherwise = Just $ SshInput
			{ hostname = val $ unMangleSshHostName host
			, username = if null user then Nothing else val user
			, directory = val dir
			}
		where
			val = Just . T.pack
			(userhost, dir) = separate (== ':') u
			(user, host) = if '@' `elem` userhost
				then separate (== '@') userhost
				else (userhost, "")

{- Test if we can ssh into the server.
 -
 - Two probe attempts are made. First, try sshing in using the existing
 - configuration, but don't let ssh prompt for any password. If
 - passwordless login is already enabled, use it. Otherwise,
 - a special ssh key will need to be generated just for this server.
 -
 - Once logged into the server, probe to see if git-annex-shell is
 - available, or rsync. Note that on OSX, ~/.ssh/git-annex-shell may be
 - present, while git-annex-shell is not in PATH.
 -}
testServer :: SshInput -> IO (Either ServerStatus SshData)
testServer (SshInput { hostname = Nothing }) = return $
	Left $ UnusableServer "Please enter a host name."
testServer sshinput@(SshInput { hostname = Just hn }) = do
	status <- probe [sshOpt "NumberOfPasswordPrompts" "0"]
	if usable status
		then ret status False
		else do
			status' <- probe []
			if usable status'
				then ret status' True
				else return $ Left status'
	where
		ret status needspubkey = return $ Right $
			(mkSshData sshinput)
				{ needsPubKey = needspubkey
				, rsyncOnly = status == UsableRsyncServer
				}
		probe extraopts = do
			let remotecommand = join ";"
				[ report "loggedin"
				, checkcommand "git-annex-shell"
				, checkcommand "rsync"
				, checkcommand osx_shim
				]
			knownhost <- knownHost hn
			let sshopts = filter (not . null) $ extraopts ++
				{- If this is an already known host, let
				 - ssh check it as usual.
				 - Otherwise, trust the host key. -}
				[ if knownhost then "" else sshOpt "StrictHostKeyChecking" "no"
				, "-n" -- don't read from stdin
				, genSshHost (fromJust $ hostname sshinput) (username sshinput)
				, remotecommand
				]
			parsetranscript . fst <$> sshTranscript sshopts ""
		parsetranscript s
			| reported "git-annex-shell" = UsableSshInput
			| reported osx_shim = UsableSshInput
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
		osx_shim = "~/.ssh/git-annex-shell"

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
getMakeSshGitR = makeSsh False setupGroup

getMakeSshRsyncR :: SshData -> Handler RepHtml
getMakeSshRsyncR = makeSsh True setupGroup

makeSsh :: Bool -> (Remote -> Handler ()) -> SshData -> Handler RepHtml
makeSsh rsync setup sshdata
	| needsPubKey sshdata = do
		keypair <- liftIO genSshKeyPair
		sshdata' <- liftIO $ setupSshKeyPair keypair sshdata
		makeSsh' rsync setup sshdata' (Just keypair)
	| otherwise = makeSsh' rsync setup sshdata Nothing

makeSsh' :: Bool -> (Remote -> Handler ()) -> SshData -> Maybe SshKeyPair -> Handler RepHtml
makeSsh' rsync setup sshdata keypair =
	sshSetup [sshhost, remoteCommand] "" $
		makeSshRepo rsync setup sshdata
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

makeSshRepo :: Bool -> (Remote -> Handler ()) -> SshData -> Handler RepHtml
makeSshRepo forcersync setup sshdata = do
	webapp <- getYesod
	r <- liftIO $ makeSshRemote
		(fromJust $ threadState webapp)
		(daemonStatus webapp)
		(scanRemotes webapp)
		forcersync sshdata
	setup r
	redirect RepositoriesR

getAddRsyncNetR :: Handler RepHtml
getAddRsyncNetR = do
	((result, form), enctype) <- runFormGet $
		renderBootstrap $ sshInputAForm $
			SshInput Nothing Nothing Nothing
	let showform status = bootstrap (Just Config) $ do
		sideBarDisplay
		setTitle "Add a Rsync.net repository"	
		let authtoken = webAppFormAuthToken
		$(widgetFile "configurators/addrsync.net")
	case result of
		FormSuccess sshinput
			| isRsyncNet (hostname sshinput) ->
				makeRsyncNet sshinput setupGroup
			| otherwise ->
				showform $ UnusableServer
					"That is not a rsync.net host name."
		_ -> showform UntestedServer

makeRsyncNet :: SshInput -> (Remote -> Handler ()) -> Handler RepHtml
makeRsyncNet sshinput setup = do
	knownhost <- liftIO $ maybe (return False) knownHost (hostname sshinput)
	keypair <- liftIO $ genSshKeyPair
	sshdata <- liftIO $ setupSshKeyPair keypair $
		(mkSshData sshinput)
			{ sshRepoName = "rsync.net"
			, needsPubKey = True
			, rsyncOnly = True
			}
	{- I'd prefer to separate commands with && , but
	 - rsync.net's shell does not support that.
	 -
	 - The dd method of appending to the authorized_keys file is the
	 - one recommended by rsync.net documentation. I touch the file first
	 - to not need to use a different method to create it.
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
	sshSetup sshopts (sshPubKey keypair) $
		makeSshRepo True setup sshdata

isRsyncNet :: Maybe Text -> Bool
isRsyncNet Nothing = False
isRsyncNet (Just host) = ".rsync.net" `T.isSuffixOf` T.toLower host

setupGroup :: Remote -> Handler ()
setupGroup r = runAnnex () $ groupSet (Remote.uuid r) (S.singleton "server")
