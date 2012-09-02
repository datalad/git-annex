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
import Assistant.WebApp.SideBar
import Utility.Yesod

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Network.BSD
import System.Posix.User
import System.Process (CreateProcess(..))

data SshServer = SshServer
	{ hostname :: Maybe Text
	, username :: Maybe Text
	, directory :: Maybe Text
	}
	deriving Show

type PubKey = String

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

usable :: ServerStatus -> Bool
usable UntestedServer = False
usable (UnusableServer _) = False
usable UsableRsyncServer = True
usable UsableSshServer = True

getAddSshR :: Handler RepHtml
getAddSshR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add a remote server"
	u <- liftIO $ T.pack . userName
		<$> (getUserEntryForID =<< getEffectiveUserID)
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ sshServerAForm u
	case result of
		FormSuccess sshserver -> do
			(status, sshserver', pubkey) <- liftIO $ testServer sshserver
			if usable status
				then error $ "TODO " ++ show sshserver'
				else showform form enctype status
		_ -> showform form enctype UntestedServer
	where
		showform form enctype status = do
			let authtoken = webAppFormAuthToken
			$(widgetFile "configurators/addssh")

		buttonText :: ServerStatus -> Text
		buttonText UsableRsyncServer = "Make rsync repository"
		buttonText UsableSshServer = "Clone repository to ssh server"
		buttonText _ = "Check this server"

		willTest UntestedServer = True
		willTest (UnusableServer _) = True
		willTest _ = False

{- Test if we can ssh into the server.
 -
 - Two probe attempts are made. First, try sshing in using the existing
 - configuration, but don't let ssh prompt for any password. If
 - passwordless login is already enabled, use it. Otherwise,
 - a special ssh key is generated just for this server.
 -
 - Once logged into the server, probe to see if git-annex-shell is
 - available, or rsync.
 -}
testServer :: SshServer -> IO (ServerStatus, SshServer, Maybe PubKey)
testServer sshserver@(SshServer { hostname = Nothing }) = return
	(UnusableServer "Please enter a host name.", sshserver, Nothing)
testServer sshserver = do
	home <- myHomeDir
	let sshdir = home </> ".ssh"
	status <- probe sshdir sshserver [sshopt "NumberOfPasswordPrompts" "0"]
	if usable status
		then return (status, sshserver, Nothing)
		else do
			(pubkey, sshserver') <- genSshKey sshdir sshserver
			status' <- probe sshdir sshserver' []
			return (status', sshserver', Just pubkey)
	where
		probe sshdir s extraopts = do
			{- This checks the unmangled server name in sshserver. -}
			knownhost <- knownHost sshdir sshserver
			let remotecommand = join ";" $
				[ report "loggedin"
				, checkcommand "git-annex-shell"
				, checkcommand "rsync"
				]
			let user = maybe "" (\u -> T.unpack u ++ "@") $ username s
			let host = user ++ T.unpack (fromJust $ hostname s)
			let sshopts = nonempty $ extraopts ++
				{- If this is an already known host, let
				 - ssh check it as usual.
				 - Otherwise, trust the host key. -}
				[ if knownhost then "" else sshopt "StrictHostKeyChecking" "no"
				, "-n" -- don't read from stdin
				, host
				, remotecommand
				]
			parsetranscript <$> sshTranscript sshopts
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

sshTranscript :: [String] -> IO String
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
	void $ waitForProcess pid
	return transcript

{- Returns the public key content, and SshServer with a mangled hostname
 - to use that will enable use of the key. This way we avoid changing the
 - user's regular ssh experience at all. -}
genSshKey :: FilePath -> SshServer -> IO (PubKey, SshServer)
genSshKey _ (SshServer { hostname = Nothing }) = undefined
genSshKey sshdir sshserver@(SshServer { hostname = Just h }) = do
	createDirectoryIfMissing True sshdir
	unlessM (doesFileExist $ sshdir </> sshprivkeyfile) $
		unlessM genkey $
			error "ssh-keygen failed"
	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $
		appendFile configfile $ unlines
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			, "\tHostname " ++ T.unpack h
			, "\tIdentityFile ~/.ssh/" ++ sshprivkeyfile
			]
	pubkey <- readFile $ sshdir </> sshpubkeyfile
	return (pubkey, sshserver { hostname = Just $ T.pack mangledhost })
	where
		configfile = sshdir </> "config"
		sshprivkeyfile = "key." ++ mangledhost
		sshpubkeyfile = sshprivkeyfile ++ ".pub"
		mangledhost = "git-annex-" ++ T.unpack h ++ user
		user = maybe "" (\u -> "-" ++ T.unpack u) (username sshserver)
		genkey = boolSystem "ssh-keygen"
			[ Param "-P", Param "" -- no password
			, Param "-f", File $ sshdir </> sshprivkeyfile
			]

{- Does ssh have known_hosts data for a hostname? -}
knownHost :: FilePath -> SshServer -> IO Bool
knownHost _ (SshServer { hostname = Nothing }) = return False
knownHost sshdir (SshServer { hostname = Just h }) =
	ifM (doesFileExist $ sshdir </> "known_hosts")
		( not . null <$> readProcess "ssh-keygen" ["-F", T.unpack h]
		, return False
		)

makeAuthorizedKeys pubkey = Just $ join ";"
	[ "mkdir -p ~/.ssh"
	, "touch ~/.ssh/authorized_keys"
	, "chmod 600 ~/.ssh/authorized_keys"
	, "echo " ++ shellEscape pubkey ++ " >>~/.ssh/authorized_keys"
	]
