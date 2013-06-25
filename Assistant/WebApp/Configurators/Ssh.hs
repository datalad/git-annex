{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.WebApp.Common
import Assistant.Ssh
import Assistant.MakeRemote
import Utility.Rsync (rsyncUrlIsShell)
import Logs.Remote
import Remote
import Logs.PreferredContent
import Types.StandardGroups
import Utility.UserInfo

import qualified Data.Text as T
import qualified Data.Map as M
import Network.Socket

sshConfigurator :: Widget -> Handler RepHtml
sshConfigurator = page "Add a remote server" (Just Configuration)

data SshInput = SshInput
	{ inputHostname :: Maybe Text
	, inputUsername :: Maybe Text
	, inputDirectory :: Maybe Text
	, inputPort :: Int
	}
	deriving (Show)

{- SshInput is only used for applicative form prompting, this converts
 - the result of such a form into a SshData. -}
mkSshData :: SshInput -> SshData
mkSshData s = SshData 
	{ sshHostName = fromMaybe "" $ inputHostname s
	, sshUserName = inputUsername s
	, sshDirectory = fromMaybe "" $ inputDirectory s
	, sshRepoName = genSshRepoName
		(T.unpack $ fromJust $ inputHostname s)
		(maybe "" T.unpack $ inputDirectory s)
	, sshPort = inputPort s
	, needsPubKey = False
	, rsyncOnly = False
	}

mkSshInput :: SshData -> SshInput
mkSshInput s = SshInput
	{ inputHostname = Just $ sshHostName s
	, inputUsername = sshUserName s
	, inputDirectory = Just $ sshDirectory s
	, inputPort = sshPort s
	}

#if MIN_VERSION_yesod(1,2,0)
sshInputAForm :: Field Handler Text -> SshInput -> AForm Handler SshInput
#else
sshInputAForm :: Field WebApp WebApp Text -> SshInput -> AForm WebApp WebApp SshInput
#endif
sshInputAForm hostnamefield def = SshInput
	<$> aopt check_hostname "Host name" (Just $ inputHostname def)
	<*> aopt check_username "User name" (Just $ inputUsername def)
	<*> aopt textField "Directory" (Just $ Just $ fromMaybe (T.pack gitAnnexAssistantDefaultDir) $ inputDirectory def)
	<*> areq intField "Port" (Just $ inputPort def)
  where
	check_username = checkBool (all (`notElem` "/:@ \t") . T.unpack)
		bad_username textField

	bad_username = "bad user name" :: Text
#ifndef __ANDROID__
	bad_hostname = "cannot resolve host name" :: Text

	check_hostname = checkM (liftIO . checkdns) hostnamefield
	checkdns t = do
		let h = T.unpack t
		let canonname = Just $ defaultHints { addrFlags = [AI_CANONNAME] }
		r <- catchMaybeIO $ getAddrInfo canonname (Just h) Nothing
		return $ case catMaybes . map addrCanonName <$> r of
			-- canonicalize input hostname if it had no dot
			Just (fullname:_)
				| '.' `elem` h -> Right t
				| otherwise -> Right $ T.pack fullname
			Just [] -> Right t
			Nothing -> Left bad_hostname
#else
	-- getAddrInfo currently broken on Android
	check_hostname = hostnamefield -- unchecked
#endif

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
getAddSshR = postAddSshR
postAddSshR :: Handler RepHtml
postAddSshR = sshConfigurator $ do
	u <- liftIO $ T.pack <$> myUserName
	((result, form), enctype) <- liftH $
		runFormPost $ renderBootstrap $ sshInputAForm textField $
			SshInput Nothing (Just u) Nothing 22
	case result of
		FormSuccess sshinput -> do
			s <- liftIO $ testServer sshinput
			case s of
				Left status -> showform form enctype status
				Right sshdata -> liftH $ redirect $ ConfirmSshR sshdata
		_ -> showform form enctype UntestedServer
  where
	showform form enctype status = $(widgetFile "configurators/ssh/add")

sshTestModal :: Widget
sshTestModal = $(widgetFile "configurators/ssh/testmodal")

{- To enable an existing rsync special remote, parse the SshInput from
 - its rsyncurl, and display a form whose only real purpose is to check
 - if ssh public keys need to be set up. From there, we can proceed with
 - the usual repo setup; all that code is idempotent.
 -
 - Note that there's no EnableSshR because ssh remotes are not special
 - remotes, and so their configuration is not shared between repositories.
 -}
getEnableRsyncR :: UUID -> Handler RepHtml
getEnableRsyncR = postEnableRsyncR
postEnableRsyncR :: UUID -> Handler RepHtml
postEnableRsyncR u = do
	m <- fromMaybe M.empty . M.lookup u <$> liftAnnex readRemoteLog
	case (parseSshRsyncUrl =<< M.lookup "rsyncurl" m, M.lookup "name" m) of
		(Just sshinput, Just reponame) -> sshConfigurator $ do
			((result, form), enctype) <- liftH $
				runFormPost $ renderBootstrap $ sshInputAForm textField sshinput
			case result of
				FormSuccess sshinput'
					| isRsyncNet (inputHostname sshinput') ->
						void $ liftH $ makeRsyncNet sshinput' reponame (const noop)
					| otherwise -> do
						s <- liftIO $ testServer sshinput'
						case s of
							Left status -> showform form enctype status
							Right sshdata -> enable sshdata
								{ sshRepoName = reponame }
				_ -> showform form enctype UntestedServer
		_ -> redirect AddSshR
  where
	showform form enctype status = do
		description <- liftAnnex $ T.pack <$> prettyUUID u
		$(widgetFile "configurators/ssh/enable")
	enable sshdata = liftH $ redirect $ ConfirmSshR $
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
			{ inputHostname = val $ unMangleSshHostName host
			, inputUsername = if null user then Nothing else val user
			, inputDirectory = val dir
			, inputPort = 22
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
 - available, or rsync. Note that, ~/.ssh/git-annex-shell may be
 - present, while git-annex-shell is not in PATH.
 -}
testServer :: SshInput -> IO (Either ServerStatus SshData)
testServer (SshInput { inputHostname = Nothing }) = return $
	Left $ UnusableServer "Please enter a host name."
testServer sshinput@(SshInput { inputHostname = Just hn }) = do
	status <- probe [sshOpt "NumberOfPasswordPrompts" "0"]
	if usable status
		then ret status False
		else do
			status' <- probe []
			if usable status'
				then ret status' True
				else return $ Left status'
  where
	ret status needspubkey = return $ Right $ (mkSshData sshinput)
		{ needsPubKey = needspubkey
		, rsyncOnly = status == UsableRsyncServer
		}
	probe extraopts = do
		let remotecommand = shellWrap $ intercalate ";"
			[ report "loggedin"
			, checkcommand "git-annex-shell"
			, checkcommand "rsync"
			, checkcommand shim
			]
		knownhost <- knownHost hn
		let sshopts = filter (not . null) $ extraopts ++
			{- If this is an already known host, let
			 - ssh check it as usual.
			 - Otherwise, trust the host key. -}
			[ if knownhost then "" else sshOpt "StrictHostKeyChecking" "no"
			, "-n" -- don't read from stdin
			, "-p", show (inputPort sshinput)
			, genSshHost
				(fromJust $ inputHostname sshinput)
				(inputUsername sshinput)
			, remotecommand
			]
		parsetranscript . fst <$> sshTranscript sshopts Nothing
	parsetranscript s
		| reported "git-annex-shell" = UsableSshInput
		| reported shim = UsableSshInput
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
	shim = "~/.ssh/git-annex-shell"

{- Runs a ssh command; if it fails shows the user the transcript,
 - and if it succeeds, runs an action. -}
sshSetup :: [String] -> String -> Handler RepHtml -> Handler RepHtml
sshSetup opts input a = do
	(transcript, ok) <- liftIO $ sshTranscript opts (Just input)
	if ok
		then a
		else showSshErr transcript

showSshErr :: String -> Handler RepHtml
showSshErr msg = sshConfigurator $
	$(widgetFile "configurators/ssh/error")

getConfirmSshR :: SshData -> Handler RepHtml
getConfirmSshR sshdata = sshConfigurator $
	$(widgetFile "configurators/ssh/confirm")

getRetrySshR :: SshData -> Handler ()
getRetrySshR sshdata = do
	s <- liftIO $ testServer $ mkSshInput sshdata
	redirect $ either (const $ ConfirmSshR sshdata) ConfirmSshR s

getMakeSshGitR :: SshData -> Handler RepHtml
getMakeSshGitR = makeSsh False setupGroup

getMakeSshRsyncR :: SshData -> Handler RepHtml
getMakeSshRsyncR = makeSsh True setupGroup

makeSsh :: Bool -> (Remote -> Handler ()) -> SshData -> Handler RepHtml
makeSsh rsync setup sshdata
	| needsPubKey sshdata = do
		keypair <- liftIO genSshKeyPair
		sshdata' <- liftIO $ setupSshKeyPair keypair sshdata
		makeSsh' rsync setup sshdata sshdata' (Just keypair)
	| sshPort sshdata /= 22 = do
		sshdata' <- liftIO $ setSshConfig sshdata []
		makeSsh' rsync setup sshdata sshdata' Nothing
	| otherwise = makeSsh' rsync setup sshdata sshdata Nothing

makeSsh' :: Bool -> (Remote -> Handler ()) -> SshData -> SshData -> Maybe SshKeyPair -> Handler RepHtml
makeSsh' rsync setup origsshdata sshdata keypair = do
	sshSetup [sshhost, remoteCommand] "" $
		makeSshRepo rsync setup sshdata
  where
	sshhost = genSshHost (sshHostName origsshdata) (sshUserName origsshdata)
	remotedir = T.unpack $ sshDirectory sshdata
	remoteCommand = shellWrap $ intercalate "&&" $ catMaybes
		[ Just $ "mkdir -p " ++ shellEscape remotedir
		, Just $ "cd " ++ shellEscape remotedir
		, if rsync then Nothing else Just "if [ ! -d .git ]; then git init --bare --shared; fi"
		, if rsync then Nothing else Just "git annex init"
		, if needsPubKey sshdata
			then addAuthorizedKeysCommand (rsyncOnly sshdata) remotedir . sshPubKey <$> keypair
			else Nothing
		]

makeSshRepo :: Bool -> (Remote -> Handler ()) -> SshData -> Handler RepHtml
makeSshRepo forcersync setup sshdata = do
	r <- liftAssistant $ makeSshRemote forcersync sshdata Nothing
	setup r
	redirect $ EditNewCloudRepositoryR $ Remote.uuid r

getAddRsyncNetR :: Handler RepHtml
getAddRsyncNetR = postAddRsyncNetR
postAddRsyncNetR :: Handler RepHtml
postAddRsyncNetR = do
	((result, form), enctype) <- runFormPost $
		renderBootstrap $ sshInputAForm hostnamefield $
			SshInput Nothing Nothing Nothing 22
	let showform status = page "Add a Rsync.net repository" (Just Configuration) $
		$(widgetFile "configurators/addrsync.net")
	case result of
		FormSuccess sshinput
			| isRsyncNet (inputHostname sshinput) -> do
				let reponame = genSshRepoName "rsync.net" 
					(maybe "" T.unpack $ inputDirectory sshinput)
				makeRsyncNet sshinput reponame setupGroup
			| otherwise ->
				showform $ UnusableServer
					"That is not a rsync.net host name."
		_ -> showform UntestedServer
  where
	hostnamefield = textField `withExpandableNote` ("Help", help)
	help = [whamlet|
<div>
  When you sign up for a Rsync.net account, you should receive an #
  email from them with the host name and user name to put here.
<div>
  The host name will be something like "usw-s001.rsync.net", and the #
  user name something like "7491"
|]

makeRsyncNet :: SshInput -> String -> (Remote -> Handler ()) -> Handler RepHtml
makeRsyncNet sshinput reponame setup = do
	knownhost <- liftIO $ maybe (return False) knownHost (inputHostname sshinput)
	keypair <- liftIO $ genSshKeyPair
	sshdata <- liftIO $ setupSshKeyPair keypair $
		(mkSshData sshinput)
			{ sshRepoName = reponame 
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
	let remotecommand = intercalate ";"
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
setupGroup r = liftAnnex $ setStandardGroup (Remote.uuid r) TransferGroup
