{- git-annex assistant ssh utilities
 -
 - Copyright 2012-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Ssh where

import Annex.Common
import Utility.Tmp
import Utility.Shell
import Utility.Rsync
import Utility.FileMode
import Utility.SshConfig
import Git.Remote

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Network.URI

data SshData = SshData
	{ sshHostName :: Text
	, sshUserName :: Maybe Text
	, sshDirectory :: Text
	, sshRepoName :: String
	, sshPort :: Int
	, needsPubKey :: Bool
	, sshCapabilities :: [SshServerCapability]
	, sshRepoUrl :: Maybe String
	}
	deriving (Read, Show, Eq)

data SshServerCapability
	= GitAnnexShellCapable -- server has git-annex-shell installed
	| GitCapable -- server has git installed
	| RsyncCapable -- server supports raw rsync access (not only via git-annex-shell)
	| PushCapable -- repo on server is set up already, and ready to accept pushes
	deriving (Read, Show, Eq)

hasCapability :: SshData -> SshServerCapability -> Bool
hasCapability d c = c `elem` sshCapabilities d

addCapability :: SshData -> SshServerCapability -> SshData
addCapability d c = d { sshCapabilities = c : sshCapabilities d }

onlyCapability :: SshData -> SshServerCapability -> Bool
onlyCapability d c = all (== c) (sshCapabilities d)

type SshPubKey = String
type SshPrivKey = String

data SshKeyPair = SshKeyPair
	{ sshPubKey :: SshPubKey
	, sshPrivKey :: SshPrivKey
	}

instance Show SshKeyPair where
	show = sshPubKey

{- ssh -ofoo=bar command-line option -}
sshOpt :: String -> String -> String
sshOpt k v = concat ["-o", k, "=", v]

{- user@host or host -}
genSshHost :: Text -> Maybe Text -> String
genSshHost host user = maybe "" (\v -> T.unpack v ++ "@") user ++ T.unpack host

{- Generates a ssh or rsync url from a SshData. -}
genSshUrl :: SshData -> String
genSshUrl sshdata = case sshRepoUrl sshdata of
	Just repourl -> repourl
	Nothing -> addtrailingslash $ T.unpack $ T.concat $
		if (onlyCapability sshdata RsyncCapable)
			then [u, h, T.pack ":", sshDirectory sshdata]
			else [T.pack "ssh://", u, h, d]
  where
	u = maybe (T.pack "") (\v -> T.concat [v, T.pack "@"]) $ sshUserName sshdata
	h = sshHostName sshdata
	d
		| T.pack "/" `T.isPrefixOf` sshDirectory sshdata = sshDirectory sshdata
		| T.pack "~/" `T.isPrefixOf` sshDirectory sshdata = T.concat [T.pack "/", sshDirectory sshdata]
		| otherwise = T.concat [T.pack "/~/", sshDirectory sshdata]
	addtrailingslash s
		| "/" `isSuffixOf` s = s
		| otherwise = s ++ "/"

{- Reverses genSshUrl -}
parseSshUrl :: String -> Maybe SshData
parseSshUrl u
	| "ssh://" `isPrefixOf` u = fromssh (drop (length "ssh://") u)
	| otherwise = fromrsync u
  where
	mkdata (userhost, dir) = Just $ SshData
		{ sshHostName = T.pack host
		, sshUserName = if null user then Nothing else Just $ T.pack user
		, sshDirectory = T.pack dir
		, sshRepoName = genSshRepoName host dir
		-- dummy values, cannot determine from url
		, sshPort = 22
		, needsPubKey = True
		, sshCapabilities = []
		, sshRepoUrl = Nothing
		}
	  where
		(user, host) = if '@' `elem` userhost
			then separate (== '@') userhost
			else ("", userhost)
	fromrsync s
		| not (rsyncUrlIsShell u) = Nothing
		| otherwise = mkdata $ separate (== ':') s
	fromssh = mkdata . break (== '/')

{- Generates a git remote name, like host_dir or host -}
genSshRepoName :: String -> FilePath -> String
genSshRepoName host dir
	| null dir = makeLegalName host
	| otherwise = makeLegalName $ host ++ "_" ++ dir

{- The output of ssh, including both stdout and stderr. -}
sshTranscript :: [String] -> (Maybe String) -> IO (String, Bool)
sshTranscript opts input = processTranscript "ssh" opts input

{- Ensure that the ssh public key doesn't include any ssh options, like
 - command=foo, or other weirdness.
 -
 - The returned version of the key has its comment removed.
 -}
validateSshPubKey :: SshPubKey -> Either String SshPubKey
validateSshPubKey pubkey
	| length (lines pubkey) == 1 = check $ words pubkey
	| otherwise = Left "too many lines in ssh public key"
  where
	check (prefix:key:_) = checkprefix prefix (unwords [prefix, key])
	check _ = err "wrong number of words in ssh public key"

	err msg = Left $ unwords [msg, pubkey]

	checkprefix prefix validpubkey
		| ssh == "ssh" && all isAlphaNum keytype = Right validpubkey
		| otherwise = err "bad ssh public key prefix"
	  where
		(ssh, keytype) = separate (== '-') prefix

addAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO Bool
addAuthorizedKeys gitannexshellonly dir pubkey = boolSystem "sh"
	[ Param "-c" , Param $ addAuthorizedKeysCommand gitannexshellonly dir pubkey ]

{- Should only be used within the same process that added the line;
 - the layout of the line is not kepy stable across versions. -}
removeAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO ()
removeAuthorizedKeys gitannexshellonly dir pubkey = do
	let keyline = authorizedKeysLine gitannexshellonly dir pubkey
	sshdir <- sshDir
	let keyfile = sshdir </> "authorized_keys"
	ls <- lines <$> readFileStrict keyfile
	viaTmp writeSshConfig keyfile $ unlines $ filter (/= keyline) ls

{- Implemented as a shell command, so it can be run on remote servers over
 - ssh.
 -
 - The ~/.ssh/git-annex-shell wrapper script is created if not already
 - present.
 -}
addAuthorizedKeysCommand :: Bool -> FilePath -> SshPubKey -> String
addAuthorizedKeysCommand gitannexshellonly dir pubkey = intercalate "&&"
	[ "mkdir -p ~/.ssh"
	, intercalate "; "
		[ "if [ ! -e " ++ wrapper ++ " ]"
		, "then (" ++ intercalate ";" (map echoval script) ++ ") > " ++ wrapper
		, "fi"
		]
	, "chmod 700 " ++ wrapper
	, "touch ~/.ssh/authorized_keys"
	, "chmod 600 ~/.ssh/authorized_keys"
	, unwords
		[ "echo"
		, shellEscape $ authorizedKeysLine gitannexshellonly dir pubkey
		, ">>~/.ssh/authorized_keys"
		]
	]
  where
	echoval v = "echo " ++ shellEscape v
	wrapper = "~/.ssh/git-annex-shell"
	script =
		[ shebang_portable
		, "set -e"
		, "if [ \"x$SSH_ORIGINAL_COMMAND\" != \"x\" ]; then"
		,   runshell "$SSH_ORIGINAL_COMMAND"
		, "else"
		,   runshell "$@"
		, "fi"
		]
	runshell var = "exec git-annex-shell -c \"" ++ var ++ "\""

authorizedKeysLine :: Bool -> FilePath -> SshPubKey -> String
authorizedKeysLine gitannexshellonly dir pubkey
	| gitannexshellonly = limitcommand ++ pubkey
	{- TODO: Locking down rsync is difficult, requiring a rather
	 - long perl script. -}
	| otherwise = pubkey
  where
	limitcommand = "command=\"env GIT_ANNEX_SHELL_DIRECTORY="++shellEscape dir++" ~/.ssh/git-annex-shell\",no-agent-forwarding,no-port-forwarding,no-X11-forwarding,no-pty "

{- Generates a ssh key pair. -}
genSshKeyPair :: IO SshKeyPair
genSshKeyPair = withTmpDir "git-annex-keygen" $ \dir -> do
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
 - mangled hostname.
 -
 - Note that the key files are put in ~/.ssh/git-annex/, rather than directly
 - in ssh because of an **INSANE** behavior of gnome-keyring: It loads
 - ~/.ssh/ANYTHING.pub, and uses them indiscriminately. But using this key
 - for a normal login to the server will force git-annex-shell to run,
 - and locks the user out. Luckily, it does not recurse into subdirectories.
 -
 - Similarly, IdentitiesOnly is set in the ssh config to prevent the
 - ssh-agent from forcing use of a different key.
 -
 - Force strict host key checking to avoid repeated prompts
 - when git-annex and git try to access the remote, if its
 - host key has changed.
 -}
installSshKeyPair :: SshKeyPair -> SshData -> IO SshData
installSshKeyPair sshkeypair sshdata = do
	sshdir <- sshDir
	createDirectoryIfMissing True $ parentDir $ sshdir </> sshPrivKeyFile sshdata

	unlessM (doesFileExist $ sshdir </> sshPrivKeyFile sshdata) $
		writeFileProtected (sshdir </> sshPrivKeyFile sshdata) (sshPrivKey sshkeypair)
	unlessM (doesFileExist $ sshdir </> sshPubKeyFile sshdata) $
		writeFile (sshdir </> sshPubKeyFile sshdata) (sshPubKey sshkeypair)

	setSshConfig sshdata
		[ ("IdentityFile", "~/.ssh/" ++ sshPrivKeyFile sshdata)
		, ("IdentitiesOnly", "yes")
		, ("StrictHostKeyChecking", "yes")
		]

sshPrivKeyFile :: SshData -> FilePath
sshPrivKeyFile sshdata = "git-annex" </> "key." ++ mangleSshHostName sshdata

sshPubKeyFile :: SshData -> FilePath
sshPubKeyFile sshdata = sshPrivKeyFile sshdata ++ ".pub"

{- Generates an installs a new ssh key pair if one is not already
 - installed. Returns the modified SshData that will use the key pair,
 - and the key pair. -}
setupSshKeyPair :: SshData -> IO (SshData, SshKeyPair)
setupSshKeyPair sshdata = do
	sshdir <- sshDir
	mprivkey <- catchMaybeIO $ readFile (sshdir </> sshPrivKeyFile sshdata)
	mpubkey <- catchMaybeIO $ readFile (sshdir </> sshPubKeyFile sshdata)
	keypair <- case (mprivkey, mpubkey) of
		(Just privkey, Just pubkey) -> return $ SshKeyPair
			{ sshPubKey = pubkey
			, sshPrivKey = privkey
			}
		_ -> genSshKeyPair
	sshdata' <- installSshKeyPair keypair sshdata
	return (sshdata', keypair)

{- Fixes git-annex ssh key pairs configured in .ssh/config 
 - by old versions to set IdentitiesOnly.
 -
 - Strategy: Search for IdentityFile lines with key.git-annex
 - in their names. These are for git-annex ssh key pairs.
 - Add the IdentitiesOnly line immediately after them, if not already
 - present.
 -}
fixSshKeyPairIdentitiesOnly :: IO ()
fixSshKeyPairIdentitiesOnly = changeUserSshConfig $ unlines . go [] . lines
  where
	go c [] = reverse c
	go c (l:[])
		| all (`isInfixOf` l) indicators = go (fixedline l:l:c) []
		| otherwise = go (l:c) []
	go c (l:next:rest)
		| all (`isInfixOf` l) indicators && not ("IdentitiesOnly" `isInfixOf` next) = 
			go (fixedline l:l:c) (next:rest)
		| otherwise = go (l:c) (next:rest)
	indicators = ["IdentityFile", "key.git-annex"]
	fixedline tmpl = takeWhile isSpace tmpl ++ "IdentitiesOnly yes"

{- Add StrictHostKeyChecking to any ssh config stanzas that were written
 - by git-annex. -}
fixUpSshRemotes :: IO ()
fixUpSshRemotes = modifyUserSshConfig (map go)
  where
	go c@(HostConfig h _)
		| "git-annex-" `isPrefixOf` h = fixupconfig c
		| otherwise = c
	go other = other

	fixupconfig c = case findHostConfigKey c "StrictHostKeyChecking" of
		Nothing -> addToHostConfig c "StrictHostKeyChecking" "yes"
		Just _ -> c

{- Setups up a ssh config with a mangled hostname.
 - Returns a modified SshData containing the mangled hostname. -}
setSshConfig :: SshData -> [(String, String)] -> IO SshData
setSshConfig sshdata config = do
	sshdir <- sshDir
	createDirectoryIfMissing True sshdir
	let configfile = sshdir </> "config"
	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $ do
		appendFile configfile $ unlines $
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			] ++ map (\(k, v) -> "\t" ++ k ++ " " ++ v)
				(settings ++ config)
		setSshConfigMode configfile

	return $ sshdata
		{ sshHostName = T.pack mangledhost
		, sshRepoUrl = replace orighost mangledhost
			<$> sshRepoUrl sshdata
		}
  where
	orighost = T.unpack $ sshHostName sshdata
	mangledhost = mangleSshHostName sshdata
	settings =
		[ ("Hostname", orighost)
		, ("Port", show $ sshPort sshdata)
		]

{- This hostname is specific to a given repository on the ssh host,
 - so it is based on the real hostname, the username, and the directory.
 -
 - The mangled hostname has the form:
 - "git-annex-realhostname-username_port_dir"
 - Note that "-" is only used in the realhostname and as a separator;
 - this is necessary to allow unMangleSshHostName to work.
 -
 - Unusual characters are url encoded, but using "." rather than "%"
 - (the latter has special meaning to ssh).
 -
 - In the username and directory, unusual characters are any
 - non-alphanumerics, other than "_"
 -
 - The real hostname is not normally encoded at all. This is done for
 - backwards compatability and to avoid unnecessary ugliness in the
 - filename. However, when it contains special characters
 - (notably ":" which cannot be used on some filesystems), it is url
 - encoded. To indicate it was encoded, the mangled hostname
 - has the form
 - "git-annex-.encodedhostname-username_port_dir"
 -}
mangleSshHostName :: SshData -> String
mangleSshHostName sshdata = intercalate "-" 
	[ "git-annex"
	, escapehostname (T.unpack (sshHostName sshdata))
	, escape extra
	]
  where
	extra = intercalate "_" $ map T.unpack $ catMaybes
		[ sshUserName sshdata
		, Just $ T.pack $ show $ sshPort sshdata
		, Just $ sshDirectory sshdata
		]
	safe c
		| isAlphaNum c = True
		| c == '_' = True
		| otherwise = False
	escape s = replace "%" "." $ escapeURIString safe s
	escapehostname s
		| all (\c -> c == '.' || safe c) s = s
		| otherwise = '.' : escape s

{- Extracts the real hostname from a mangled ssh hostname. -}
unMangleSshHostName :: String -> String
unMangleSshHostName h = case split "-" h of
	("git":"annex":rest) -> unescape (intercalate "-" (beginning rest))
	_ -> h
  where
	unescape ('.':s) = unEscapeString (replace "." "%" s)
	unescape s = s

{- Does ssh have known_hosts data for a hostname? -}
knownHost :: Text -> IO Bool
knownHost hostname = do
	sshdir <- sshDir
	ifM (doesFileExist $ sshdir </> "known_hosts")
		( not . null <$> checkhost
		, return False
		)
  where
	{- ssh-keygen -F can crash on some old known_hosts file -}
	checkhost = catchDefaultIO "" $
		readProcess "ssh-keygen" ["-F", T.unpack hostname]
