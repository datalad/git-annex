{- git-annex assistant webapp configurator for ssh-based remotes
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Ssh where

import Assistant.WebApp.Common
import Assistant.WebApp.Gpg
import Assistant.Ssh
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

import qualified Data.Text as T
import qualified Data.Map as M
import Network.Socket
import Data.Ord

sshConfigurator :: Widget -> Handler Html
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
	, sshCapabilities = [] -- untested
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
		runFormPostNoToken $ renderBootstrap $ sshInputAForm textField $
			SshInput Nothing (Just username) Nothing 22
	case result of
		FormSuccess sshinput -> do
			s <- liftIO $ testServer sshinput
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
  	enablegcrypt sshdata _ = prepSsh True sshdata $ \sshdata' ->
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
				runFormPostNoToken $ renderBootstrap $ sshInputAForm textField sshinput
			case result of
				FormSuccess sshinput'
					| isRsyncNet (inputHostname sshinput') ->
						void $ liftH $ rsyncnetsetup sshinput' reponame
					| otherwise -> do
						s <- liftIO $ testServer sshinput'
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

{- Test if we can ssh into the server.
 -
 - Two probe attempts are made. First, try sshing in using the existing
 - configuration, but don't let ssh prompt for any password. If
 - passwordless login is already enabled, use it. Otherwise,
 - a special ssh key will need to be generated just for this server.
 -
 - Once logged into the server, probe to see if git-annex-shell,
 - git, and rsync are available. 
 - Note that, ~/.ssh/git-annex-shell may be
 - present, while git-annex-shell is not in PATH.
 -
 - Also probe to see if there is already a git repository at the location
 - with either an annex-uuid or a gcrypt-id set. (If not, returns NoUUID.)
 -}
testServer :: SshInput -> IO (Either ServerStatus (SshData, UUID))
testServer (SshInput { inputHostname = Nothing }) = return $
	Left $ UnusableServer "Please enter a host name."
testServer sshinput@(SshInput { inputHostname = Just hn }) = do
	(status, u) <- probe [sshOpt "NumberOfPasswordPrompts" "0"]
	case capabilities status of
		[] -> do
			(status', u') <- probe []
			case capabilities status' of
				[] -> return $ Left status'
				cs -> ret cs True u'
		cs -> ret cs False u
  where
	ret cs needspubkey u = do
		let sshdata = (mkSshData sshinput)
			{ needsPubKey = needspubkey
			, sshCapabilities = cs
			}
		return $ Right (sshdata, u)
	probe extraopts = do
		let remotecommand = shellWrap $ intercalate ";"
			[ report "loggedin"
			, checkcommand "git-annex-shell"
			, checkcommand "git"
			, checkcommand "rsync"
			, checkcommand shim
			, getgitconfig (T.unpack <$> inputDirectory sshinput)
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
	parsetranscript s =
		let cs = map snd $ filter (reported . fst)
			[ ("git-annex-shell", GitAnnexShellCapable)
			, (shim, GitAnnexShellCapable)
			, ("git", GitCapable)
			, ("rsync", RsyncCapable)
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
	report r = "echo " ++ token r
	shim = "~/.ssh/git-annex-shell"
	getgitconfig (Just d)
		| not (null d) = "cd " ++ shellEscape d ++ " && git config --list"
	getgitconfig _ = "echo"

{- Runs a ssh command; if it fails shows the user the transcript,
 - and if it succeeds, runs an action. -}
sshSetup :: [String] -> Maybe String -> Handler Html -> Handler Html
sshSetup opts input a = do
	(transcript, ok) <- liftIO $ sshTranscript opts input
	if ok
		then a
		else showSshErr transcript

showSshErr :: String -> Handler Html
showSshErr msg = sshConfigurator $
	$(widgetFile "configurators/ssh/error")

{- The UUID will be NoUUID when the repository does not already exist. -}
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
	s <- liftIO $ testServer $ mkSshInput sshdata
	redirect $ either (const $ ConfirmSshR sshdata NoUUID) (uncurry ConfirmSshR) s

getMakeSshGitR :: SshData -> Handler Html
getMakeSshGitR sshdata = prepSsh False sshdata makeSshRepo

getMakeSshRsyncR :: SshData -> Handler Html
getMakeSshRsyncR sshdata = prepSsh False (rsyncOnly sshdata) makeSshRepo

rsyncOnly :: SshData -> SshData
rsyncOnly sshdata = sshdata { sshCapabilities = [RsyncCapable] }

getMakeSshGCryptR :: SshData -> RepoKey -> Handler Html
getMakeSshGCryptR sshdata NoRepoKey = whenGcryptInstalled $
	withNewSecretKey $ getMakeSshGCryptR sshdata . RepoKey
getMakeSshGCryptR sshdata (RepoKey keyid) = whenGcryptInstalled $
	prepSsh True sshdata $ makeGCryptRepo keyid
	
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
prepSsh newgcrypt sshdata a
	| needsPubKey sshdata = do
		keypair <- liftIO genSshKeyPair
		sshdata' <- liftIO $ setupSshKeyPair keypair sshdata
		prepSsh' newgcrypt sshdata sshdata' (Just keypair) a
	| sshPort sshdata /= 22 = do
		sshdata' <- liftIO $ setSshConfig sshdata []
		prepSsh' newgcrypt sshdata sshdata' Nothing a
	| otherwise = prepSsh' newgcrypt sshdata sshdata Nothing a

prepSsh' :: Bool -> SshData -> SshData -> Maybe SshKeyPair -> (SshData -> Handler Html) -> Handler Html
prepSsh' newgcrypt origsshdata sshdata keypair a = sshSetup
	 [ "-p", show (sshPort origsshdata)
	 , genSshHost (sshHostName origsshdata) (sshUserName origsshdata)
	 , remoteCommand
	 ] Nothing (a sshdata)
  where
	remotedir = T.unpack $ sshDirectory sshdata
	remoteCommand = shellWrap $ intercalate "&&" $ catMaybes
		[ Just $ "mkdir -p " ++ shellEscape remotedir
		, Just $ "cd " ++ shellEscape remotedir
		, if rsynconly then Nothing else Just "if [ ! -d .git ]; then git init --bare --shared && git config receive.denyNonFastforwards false; fi"
		, if rsynconly || newgcrypt then Nothing else Just "git annex init"
		, if needsPubKey origsshdata
			then addAuthorizedKeysCommand (hasCapability origsshdata GitAnnexShellCapable) remotedir . sshPubKey <$> keypair
			else Nothing
		]
	rsynconly = onlyCapability origsshdata RsyncCapable

makeSshRepo :: SshData -> Handler Html
makeSshRepo sshdata = setupCloudRemote TransferGroup Nothing $
	makeSshRemote sshdata

makeGCryptRepo :: KeyId -> SshData -> Handler Html
makeGCryptRepo keyid sshdata = setupCloudRemote TransferGroup Nothing $ 
	makeGCryptRemote (sshRepoName sshdata) (genSshUrl sshdata) keyid

getAddRsyncNetR :: Handler Html
getAddRsyncNetR = postAddRsyncNetR
postAddRsyncNetR :: Handler Html
postAddRsyncNetR = do
	((result, form), enctype) <- runFormPostNoToken $
		renderBootstrap $ sshInputAForm hostnamefield $
			SshInput Nothing Nothing Nothing 22
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
	sshSetup [sshhost, gitinit] Nothing $ makeGCryptRepo keyid sshdata
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

{- Prepares rsync.net ssh key, and if successful, runs an action with
 - its SshData. -}
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
	sshSetup sshopts (Just $ sshPubKey keypair) $ a sshdata

isRsyncNet :: Maybe Text -> Bool
isRsyncNet Nothing = False
isRsyncNet (Just host) = ".rsync.net" `T.isSuffixOf` T.toLower host
