{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.WebApp.Common
import Assistant.WebApp.Gpg
import Assistant.Ssh
import Annex.Ssh
import Assistant.WebApp.MakeRemote
import Logs.Remote
import Remote
import Types.StandardGroups
import Utility.UserInfo
import Utility.Gpg
import Types.Remote (RemoteConfig)
import Git.Types (RemoteName)
import qualified Remote.GCrypt as GCrypt
import Annex.UUID
import Logs.UUID
import Assistant.RemoteControl
import Assistant.CredPairCache
import Config.Files
import Utility.Tmp
import Utility.FileMode
import Utility.ThreadScheduler
import Utility.Env

#ifdef mingw32_HOST_OS
import Utility.Rsync
#endif

import qualified Data.Text as T
import qualified Data.Map as M
import Network.Socket
import Data.Ord

sshConfigurator :: Widget -> Handler Html
sshConfigurator = page "Add a remote server" (Just Configuration)

data SshInput = SshInput
	{ inputHostname :: Maybe Text
	, inputUsername :: Maybe Text
	, inputAuthMethod :: AuthMethod
	, inputPassword :: Maybe Text
	, inputDirectory :: Maybe Text
	, inputPort :: Int
	}

data AuthMethod
	= Password
	| CachedPassword
	| ExistingSshKey
	deriving (Eq, Show)

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
	, sshCapabilities = [] -- untested
	}

mkSshInput :: SshData -> SshInput
mkSshInput s = SshInput
	{ inputHostname = Just $ sshHostName s
	, inputUsername = sshUserName s
	, inputAuthMethod = if needsPubKey s then CachedPassword else ExistingSshKey
	, inputPassword = Nothing
	, inputDirectory = Just $ sshDirectory s
	, inputPort = sshPort s
	}

#if MIN_VERSION_yesod(1,2,0)
sshInputAForm :: Field Handler Text -> SshInput -> AForm Handler SshInput
#else
sshInputAForm :: Field WebApp WebApp Text -> SshInput -> AForm WebApp WebApp SshInput
#endif
sshInputAForm hostnamefield def = SshInput
	<$> aopt check_hostname (bfs "Host name") (Just $ inputHostname def)
	<*> aopt check_username (bfs "User name") (Just $ inputUsername def)
	<*> areq (selectFieldList authmethods) (bfs "Authenticate with") (Just $ inputAuthMethod def)
	<*> aopt passwordField (bfs "Password") Nothing
	<*> aopt textField (bfs "Directory") (Just $ Just $ fromMaybe (T.pack gitAnnexAssistantDefaultDir) $ inputDirectory def)
	<*> areq intField (bfs "Port") (Just $ inputPort def)
  where
	authmethods :: [(Text, AuthMethod)]
	authmethods =
		[ ("password", Password)
		, ("existing ssh key", ExistingSshKey)
		]

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
		return $ case mapMaybe addrCanonName <$> r of
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
	| UsableServer [SshServerCapability]
	deriving (Eq)

capabilities :: ServerStatus -> [SshServerCapability]
capabilities (UsableServer cs) = cs
capabilities _ = []

getAddSshR :: Handler Html
getAddSshR = postAddSshR
postAddSshR :: Handler Html
postAddSshR = sshConfigurator $ do
	username <- liftIO $ T.pack <$> myUserName
	((result, form), enctype) <- liftH $
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ sshInputAForm textField $
			SshInput Nothing (Just username) Password Nothing Nothing 22
	case result of
		FormSuccess sshinput -> do
			s <- liftAssistant $ testServer sshinput
			case s of
				Left status -> showform form enctype status
				Right (sshdata, u) -> liftH $ redirect $ ConfirmSshR sshdata u
		_ -> showform form enctype UntestedServer
  where
	showform form enctype status = $(widgetFile "configurators/ssh/add")

sshTestModal :: Widget
sshTestModal = $(widgetFile "configurators/ssh/testmodal")

sshSetupModal :: SshData -> Widget
sshSetupModal sshdata = $(widgetFile "configurators/ssh/setupmodal")

getEnableRsyncR :: UUID -> Handler Html
getEnableRsyncR = postEnableRsyncR
postEnableRsyncR :: UUID -> Handler Html
postEnableRsyncR = enableSpecialSshRemote getsshinput enableRsyncNet enablersync
  where
	enablersync sshdata u = redirect $ ConfirmSshR
		(sshdata { sshCapabilities = [RsyncCapable] }) u
	getsshinput = parseSshUrl <=< M.lookup "rsyncurl"

{- This only handles gcrypt repositories that are located on ssh servers;
 - ones on local drives are handled via another part of the UI. -}
getEnableSshGCryptR :: UUID -> Handler Html
getEnableSshGCryptR = postEnableSshGCryptR
postEnableSshGCryptR :: UUID -> Handler Html
postEnableSshGCryptR u = whenGcryptInstalled $
	enableSpecialSshRemote getsshinput enableRsyncNetGCrypt enablegcrypt u
  where
  	enablegcrypt sshdata _ = prepSsh False sshdata $ \sshdata' ->
		sshConfigurator $
			checkExistingGCrypt sshdata' $
				error "Expected to find an encrypted git repository, but did not."
	getsshinput = parseSshUrl <=< M.lookup "gitrepo"

{- To enable a special remote that uses ssh as its transport, 
 - parse a config key to get its url, and display a form whose
 - only real purpose is to check if ssh public keys need to be
 - set up.
 -}
enableSpecialSshRemote :: (RemoteConfig -> Maybe SshData) -> (SshInput -> RemoteName -> Handler Html) -> (SshData -> UUID -> Handler Html) -> UUID -> Handler Html
enableSpecialSshRemote getsshinput rsyncnetsetup genericsetup u = do
	m <- fromMaybe M.empty . M.lookup u <$> liftAnnex readRemoteLog
	case (mkSshInput . unmangle <$> getsshinput m, M.lookup "name" m) of
		(Just sshinput, Just reponame) -> sshConfigurator $ do
			((result, form), enctype) <- liftH $
				runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ sshInputAForm textField sshinput
			case result of
				FormSuccess sshinput'
					| isRsyncNet (inputHostname sshinput') ->
						void $ liftH $ rsyncnetsetup sshinput' reponame
					| otherwise -> do
						s <- liftAssistant $ testServer sshinput'
						case s of
							Left status -> showform form enctype status
							Right (sshdata, _u) -> void $ liftH $ genericsetup
								( sshdata { sshRepoName = reponame } ) u
				_ -> showform form enctype UntestedServer
		_ -> redirect AddSshR
  where
  	unmangle sshdata = sshdata
		{ sshHostName = T.pack $ unMangleSshHostName $
			T.unpack $ sshHostName sshdata
		}
	showform form enctype status = do
		description <- liftAnnex $ T.pack <$> prettyUUID u
		$(widgetFile "configurators/ssh/enable")

{- To deal with git-annex and possibly even git and rsync not being
 - available in the remote server's PATH, when git-annex was installed
 - from the standalone tarball etc, look for a ~/.ssh/git-annex-wrapper
 - and if it's there, use it to run a command. -}
wrapCommand :: String -> String
wrapCommand cmd = "if [ -x " ++ commandWrapper ++ " ]; then " ++ commandWrapper ++ " " ++ cmd ++ "; else " ++ cmd ++ "; fi"

commandWrapper :: String
commandWrapper = "~/.ssh/git-annex-wrapper"

{- Test if we can ssh into the server, using the specified AuthMethod.
 -
 - Once logged into the server, probe to see if git-annex-shell,
 - git, and rsync are available. 
 -
 - Note that ~/.ssh/git-annex-shell may be present, while
 - git-annex-shell is not in PATH.
 - Also, git and rsync may not be in PATH; as long as the commandWrapper
 - is present, assume it is able to be used to run them.
 -
 - Also probe to see if there is already a git repository at the location
 - with either an annex-uuid or a gcrypt-id set. (If not, returns NoUUID.)
 -}
testServer :: SshInput -> Assistant (Either ServerStatus (SshData, UUID))
testServer (SshInput { inputHostname = Nothing }) = return $
	Left $ UnusableServer "Please enter a host name."
testServer sshinput@(SshInput { inputHostname = Just hn }) = do
	(status, u) <- probe
	case capabilities status of
		[] -> return $ Left status
		cs -> do
			let sshdata = (mkSshData sshinput)
				{ needsPubKey = inputAuthMethod sshinput /= ExistingSshKey
				, sshCapabilities = cs
				}
			return $ Right (sshdata, u)
  where
	probe = do
		let remotecommand = shellWrap $ intercalate ";"
			[ report "loggedin"
			, checkcommand "git-annex-shell"
			, checkcommand "git"
			, checkcommand "rsync"
			, checkcommand shim
			, checkcommand commandWrapper
			, getgitconfig (T.unpack <$> inputDirectory sshinput)
			]
		knownhost <- liftIO $ knownHost hn
		let sshopts = catMaybes
			{- If this is an already known host, let
			 - ssh check it as usual.
			 - Otherwise, trust the host key. -}
			[ if knownhost then Nothing else Just (sshOpt "StrictHostKeyChecking" "no")
			, Just "-n" -- don't read from stdin
			, Just "-p", Just (show (inputPort sshinput))
			, Just $ genSshHost
				(fromJust $ inputHostname sshinput)
				(inputUsername sshinput)
			, Just remotecommand
			]
		parsetranscript . fst <$> sshAuthTranscript sshinput sshopts Nothing
	parsetranscript s =
		let cs = map snd $ filter (reported . fst)
			[ ("git-annex-shell", GitAnnexShellCapable)
			, (shim, GitAnnexShellCapable)
			, ("git", GitCapable)
			, ("rsync", RsyncCapable)
			, (commandWrapper, GitCapable)
			, (commandWrapper, RsyncCapable)
			]
		    u = fromMaybe NoUUID $ headMaybe $ mapMaybe finduuid $
			map (separate (== '=')) $ lines s
		in if null cs
			then (UnusableServer unusablereason, u)
			else (UsableServer cs, u)
	  where
		reported r = token r `isInfixOf` s
		unusablereason = if reported "loggedin"
				then "Neither rsync nor git-annex are installed on the server. Perhaps you should go install them?"
				else T.pack $ "Failed to ssh to the server. Transcript: " ++ s
		finduuid (k, v)
			| k == "annex.uuid" = Just $ toUUID v
			| k == GCrypt.coreGCryptId = Just $ genUUIDInNameSpace gCryptNameSpace v
			| otherwise = Nothing
	
	checkcommand c = "if which " ++ c ++ "; then " ++ report c ++ "; fi"
	token r = "git-annex-probe " ++ r
	report r = "echo " ++ shellEscape (token r)
	shim = "~/.ssh/git-annex-shell"
	getgitconfig (Just d)
		| not (null d) = "cd " ++ shellEscape d ++ " && git config --list"
	getgitconfig _ = "echo"

{- Runs a ssh command to set up the repository; if it fails shows
 - the user the transcript, and if it succeeds, runs an action. -}
sshSetup :: SshInput -> [String] -> Maybe String -> Handler Html -> Handler Html
sshSetup sshinput opts input a = do
	(transcript, ok) <- liftAssistant $ sshAuthTranscript sshinput opts input
	if ok
		then a
		else showSshErr transcript

showSshErr :: String -> Handler Html
showSshErr msg = sshConfigurator $
	$(widgetFile "configurators/ssh/error")

{- Runs a ssh command, returning a transcript of its output.
 -
 - Depending on the SshInput, avoids using a password, or uses a
 - cached password. ssh is coaxed to use git-annex as SSH_ASKPASS
 - to get the password.
 -
 - Note that ssh will only use SSH_ASKPASS when DISPLAY is set and there
 - is no controlling terminal. On Unix, that is set up when the assistant
 - starts, by calling createSession. On Windows, all of stdin, stdout, and
 - stderr must be disconnected from the terminal. This is accomplished
 - by always providing input on stdin.
 -}
sshAuthTranscript :: SshInput -> [String] -> (Maybe String) -> Assistant (String, Bool)
sshAuthTranscript sshinput opts input = case inputAuthMethod sshinput of
	ExistingSshKey -> liftIO $ go [passwordprompts 0] Nothing
	CachedPassword -> setupAskPass
	Password -> do
		cacheCred (login, geti inputPassword) (Seconds $ 60 * 10)
		setupAskPass
  where
	login = geti inputUsername ++ "@" ++ geti inputHostname
	geti f = maybe "" T.unpack (f sshinput)

	go extraopts env = processTranscript' "ssh" (extraopts ++ opts) env $
		Just (fromMaybe "" input)

	setupAskPass = do
		program <- liftIO readProgramFile
		v <- getCachedCred login
		liftIO $ case v of
			Nothing -> go [passwordprompts 0] Nothing
			Just pass -> withTmpFile "ssh" $ \passfile h -> do
				hClose h
				writeFileProtected passfile pass
				env <- getEnvironment
				let env' = addEntries
					[ ("SSH_ASKPASS", program)
					, (sshAskPassEnv, passfile)
					-- ssh does not use SSH_ASKPASS
					-- unless DISPLAY is set, and
					-- there is no controlling
					-- terminal.
					, ("DISPLAY", ":0")
					] env
				go [passwordprompts 1] (Just env')
	
	passwordprompts :: Int -> String
	passwordprompts = sshOpt "NumberOfPasswordPrompts" . show

{- The UUID will be NoUUID when the repository does not already exist,
 - or was not a git-annex repository before. -}
getConfirmSshR :: SshData -> UUID -> Handler Html
getConfirmSshR sshdata u
	| u == NoUUID = handlenew
	| otherwise = handleexisting =<< (M.lookup u <$> liftAnnex uuidMap)
  where
	handlenew = sshConfigurator $ do
		secretkeys <- sortBy (comparing snd) . M.toList
			<$> liftIO secretKeys
		$(widgetFile "configurators/ssh/confirm")
  	handleexisting Nothing = sshConfigurator $
		-- Not a UUID we know, so prompt about combining.
		$(widgetFile "configurators/ssh/combine")
	handleexisting (Just _) = prepSsh False sshdata $ \sshdata' -> do
		m <- liftAnnex readRemoteLog
		case M.lookup "type" =<< M.lookup u m of
			Just "gcrypt" -> combineExistingGCrypt sshdata' u
			-- This handles enabling git repositories
			-- that already exist.
			_ -> makeSshRepo sshdata'

{- The user has confirmed they want to combine with a ssh repository,
 - which is not known to us. So it might be using gcrypt. -}
getCombineSshR :: SshData -> Handler Html
getCombineSshR sshdata = prepSsh False sshdata $ \sshdata' ->
	sshConfigurator $
		checkExistingGCrypt sshdata' $
			void $ liftH $ makeSshRepo sshdata'

getRetrySshR :: SshData -> Handler ()
getRetrySshR sshdata = do
	s <- liftAssistant $ testServer $ mkSshInput sshdata
	redirect $ either (const $ ConfirmSshR sshdata NoUUID) (uncurry ConfirmSshR) s

{- Making a new git repository. -}
getMakeSshGitR :: SshData -> Handler Html
getMakeSshGitR sshdata = prepSsh True sshdata makeSshRepo

getMakeSshRsyncR :: SshData -> Handler Html
getMakeSshRsyncR sshdata = prepSsh False (rsyncOnly sshdata) makeSshRepo

rsyncOnly :: SshData -> SshData
rsyncOnly sshdata = sshdata { sshCapabilities = [RsyncCapable] }

getMakeSshGCryptR :: SshData -> RepoKey -> Handler Html
getMakeSshGCryptR sshdata NoRepoKey = whenGcryptInstalled $
	withNewSecretKey $ getMakeSshGCryptR sshdata . RepoKey
getMakeSshGCryptR sshdata (RepoKey keyid) = whenGcryptInstalled $
	prepSsh False sshdata $ makeGCryptRepo keyid
	
{- Detect if the user entered a location with an existing, known
 - gcrypt repository, and enable it. Otherwise, runs the action. -}
checkExistingGCrypt :: SshData -> Widget -> Widget
checkExistingGCrypt sshdata nope = checkGCryptRepoEncryption repourl nope nope $ do
	mu <- liftAnnex $ probeGCryptRemoteUUID repourl
	case mu of
		Just u -> void $ liftH $
			combineExistingGCrypt sshdata u
		Nothing -> error "The location contains a gcrypt repository that is not a git-annex special remote. This is not supported."
  where
  	repourl = genSshUrl sshdata

{- Enables an existing gcrypt special remote. -}
enableGCrypt :: SshData -> RemoteName -> Handler Html
enableGCrypt sshdata reponame = 
	setupCloudRemote TransferGroup Nothing $ 
		enableSpecialRemote reponame GCrypt.remote Nothing $ M.fromList
			[("gitrepo", genSshUrl sshdata)]

{- Combining with a gcrypt repository that may not be
 - known in remote.log, so probe the gcrypt repo. -}
combineExistingGCrypt :: SshData -> UUID -> Handler Html
combineExistingGCrypt sshdata u = do
	reponame <- liftAnnex $ getGCryptRemoteName u repourl
	enableGCrypt sshdata reponame
  where
  	repourl = genSshUrl sshdata

{- Sets up remote repository for ssh, or directory for rsync. -}
prepSsh :: Bool -> SshData -> (SshData -> Handler Html) -> Handler Html
prepSsh needsinit sshdata a
	| needsPubKey sshdata = do
		keypair <- liftIO genSshKeyPair
		sshdata' <- liftIO $ setupSshKeyPair keypair sshdata
		prepSsh' needsinit sshdata sshdata' (Just keypair) a
	| sshPort sshdata /= 22 = do
		sshdata' <- liftIO $ setSshConfig sshdata []
		prepSsh' needsinit sshdata sshdata' Nothing a
	| otherwise = prepSsh' needsinit sshdata sshdata Nothing a

prepSsh' :: Bool -> SshData -> SshData -> Maybe SshKeyPair -> (SshData -> Handler Html) -> Handler Html
prepSsh' needsinit origsshdata sshdata keypair a = sshSetup (mkSshInput origsshdata)
	 [ "-p", show (sshPort origsshdata)
	 , genSshHost (sshHostName origsshdata) (sshUserName origsshdata)
	 , remoteCommand
	 ] Nothing (a sshdata)
  where
	remotedir = T.unpack $ sshDirectory sshdata
	remoteCommand = shellWrap $ intercalate "&&" $ catMaybes
		[ Just $ "mkdir -p " ++ shellEscape remotedir
		, Just $ "cd " ++ shellEscape remotedir
		, if rsynconly then Nothing else Just $ unwords
			[ "if [ ! -d .git ]; then"
			, wrapCommand "git init --bare --shared"
		, "&&"
			, wrapCommand "git config receive.denyNonFastforwards"
			, ";fi"
			]
		, if needsinit then Just (wrapCommand "git annex init") else Nothing
		, if needsPubKey origsshdata
			then addAuthorizedKeysCommand (hasCapability origsshdata GitAnnexShellCapable) remotedir . sshPubKey <$> keypair
			else Nothing
		]
	rsynconly = onlyCapability origsshdata RsyncCapable

makeSshRepo :: SshData -> Handler Html
makeSshRepo sshdata
	| onlyCapability sshdata RsyncCapable = setupCloudRemote TransferGroup Nothing go
	| otherwise = makeSshRepoConnection go
  where
	go = makeSshRemote sshdata

makeSshRepoConnection :: Annex RemoteName -> Handler Html
makeSshRepoConnection a = setupRemote postsetup TransferGroup Nothing a
  where
	postsetup u = do
		liftAssistant $ sendRemoteControl RELOAD
		redirect $ EditNewRepositoryR u

makeGCryptRepo :: KeyId -> SshData -> Handler Html
makeGCryptRepo keyid sshdata = makeSshRepoConnection $ 
	makeGCryptRemote (sshRepoName sshdata) (genSshUrl sshdata) keyid

getAddRsyncNetR :: Handler Html
getAddRsyncNetR = postAddRsyncNetR
postAddRsyncNetR :: Handler Html
postAddRsyncNetR = do
	((result, form), enctype) <- runFormPostNoToken $
		renderBootstrap3 bootstrapFormLayout $ sshInputAForm hostnamefield $
			SshInput Nothing Nothing Password Nothing Nothing 22
	let showform status = inpage $
		$(widgetFile "configurators/rsync.net/add")
	case result of
		FormSuccess sshinput
			| isRsyncNet (inputHostname sshinput) ->
				go sshinput
			| otherwise ->
				showform $ UnusableServer
					"That is not a rsync.net host name."
		_ -> showform UntestedServer
  where
  	inpage = page "Add a Rsync.net repository" (Just Configuration)
	hostnamefield = textField `withExpandableNote` ("Help", help)
	help = [whamlet|
<div>
  When you sign up for a Rsync.net account, you should receive an #
  email from them with the host name and user name to put here.
<div>
  The host name will be something like "usw-s001.rsync.net", and the #
  user name something like "7491"
|]
	go sshinput = do
		let reponame = genSshRepoName "rsync.net" 
			(maybe "" T.unpack $ inputDirectory sshinput)
		
		prepRsyncNet sshinput reponame $ \sshdata -> inpage $ 
			checkExistingGCrypt sshdata $ do
				secretkeys <- sortBy (comparing snd) . M.toList
					<$> liftIO secretKeys
				$(widgetFile "configurators/rsync.net/encrypt")

getMakeRsyncNetSharedR :: SshData -> Handler Html
getMakeRsyncNetSharedR = makeSshRepo . rsyncOnly

{- Make a gcrypt special remote on rsync.net. -}
getMakeRsyncNetGCryptR :: SshData -> RepoKey -> Handler Html
getMakeRsyncNetGCryptR sshdata NoRepoKey = whenGcryptInstalled $
	withNewSecretKey $ getMakeRsyncNetGCryptR sshdata . RepoKey
getMakeRsyncNetGCryptR sshdata (RepoKey keyid) = whenGcryptInstalled $
	sshSetup (mkSshInput sshdata) [sshhost, gitinit] Nothing $ makeGCryptRepo keyid sshdata
  where
	sshhost = genSshHost (sshHostName sshdata) (sshUserName sshdata)
	gitinit = "git init --bare " ++ T.unpack (sshDirectory sshdata)

enableRsyncNet :: SshInput -> String -> Handler Html
enableRsyncNet sshinput reponame = 
	prepRsyncNet sshinput reponame $ makeSshRepo . rsyncOnly

enableRsyncNetGCrypt :: SshInput -> RemoteName -> Handler Html
enableRsyncNetGCrypt sshinput reponame = 
	prepRsyncNet sshinput reponame $ \sshdata -> whenGcryptInstalled $
		checkGCryptRepoEncryption (genSshUrl sshdata) notencrypted notinstalled $
			enableGCrypt sshdata reponame
  where
	notencrypted = error "Unexpectedly found a non-encrypted git repository, instead of the expected encrypted git repository."
	notinstalled = error "internal"

{- Prepares rsync.net ssh key and creates the directory that will be 
 - used on rsync.net. If successful, runs an action with its SshData.
 -
 - To append the ssh key to rsync.net's authorized_keys, their
 - documentation recommends a dd methodd, where the line is fed
 - in to ssh over stdin.
 -
 - On Windows, ssh password prompting happens on stdin, so cannot
 - feed the key in that way. Instead, first rsync down any current
 - authorized_keys file, then modifiy it, and then rsync it back up.
 - This means 2 password prompts rather than one for Windows.
 -}
prepRsyncNet :: SshInput -> String -> (SshData -> Handler Html) -> Handler Html
prepRsyncNet sshinput reponame a = do
	knownhost <- liftIO $ maybe (return False) knownHost (inputHostname sshinput)
	keypair <- liftIO genSshKeyPair
	sshdata <- liftIO $ setupSshKeyPair keypair $
		(mkSshData sshinput)
			{ sshRepoName = reponame 
			, needsPubKey = True
			, sshCapabilities = [RsyncCapable]
			}
	let sshhost = genSshHost (sshHostName sshdata) (sshUserName sshdata)
	let torsyncnet cmd = filter (not . null)
		[ if knownhost then "" else sshOpt "StrictHostKeyChecking" "no"
		, sshhost
		, cmd
		]
#ifndef mingw32_HOST_OS
	{- I'd prefer to separate commands with && , but
	 - rsync.net's shell does not support that. -}
	let remotecommand = intercalate ";"
		[ "mkdir -p .ssh"
		, "touch .ssh/authorized_keys"
		, "dd of=.ssh/authorized_keys oflag=append conv=notrunc"
		, "mkdir -p " ++ T.unpack (sshDirectory sshdata)
		]
	sshSetup sshinput (torsyncnet remotecommand) (Just $ sshPubKey keypair) (a sshdata)
#else
	liftIO $ withTmpDir "rsyncnet" $ \tmpdir -> do
		createDirectory $ tmpdir </> ".ssh"
		(oldkeys, _) <- sshTranscript (torsyncnet "cat .ssh/authorized_keys") Nothing
		writeFile (tmpdir </> ".ssh" </> "authorized_keys")
			(sshPubKey keypair ++ "\n" ++ oldkeys)
		void $ rsync
			[ Param "-r"
			, File $ tmpdir </> ".ssh/"
			, Param $ sshhost ++ ":.ssh/"
			]
	let remotecommand = "mkdir -p " ++ T.unpack (sshDirectory sshdata)
	sshSetup sshinput (torsyncnet remotecommand) Nothing (a sshdata)
#endif

isRsyncNet :: Maybe Text -> Bool
isRsyncNet Nothing = False
isRsyncNet (Just host) = ".rsync.net" `T.isSuffixOf` T.toLower host
