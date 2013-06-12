{- git-annex assistant ssh utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Ssh where

import Common.Annex
import Utility.Tmp
import Utility.UserInfo
import Utility.Shell
import Git.Remote

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

data SshData = SshData
	{ sshHostName :: Text
	, sshUserName :: Maybe Text
	, sshDirectory :: Text
	, sshRepoName :: String
	, sshPort :: Int
	, needsPubKey :: Bool
	, rsyncOnly :: Bool
	}
	deriving (Read, Show, Eq)

data SshKeyPair = SshKeyPair
	{ sshPubKey :: String
	, sshPrivKey :: String
	}

instance Show SshKeyPair where
	show = sshPubKey

type SshPubKey = String

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

{- Generates a git remote name, like host_dir or host -}
genSshRepoName :: String -> FilePath -> String
genSshRepoName host dir
	| null dir = makeLegalName host
	| otherwise = makeLegalName $ host ++ "_" ++ dir

{- The output of ssh, including both stdout and stderr. -}
sshTranscript :: [String] -> (Maybe String) -> IO (String, Bool)
sshTranscript opts input = processTranscript "ssh" opts input

{- Ensure that the ssh public key doesn't include any ssh options, like
 - command=foo, or other weirdness -}
validateSshPubKey :: SshPubKey -> IO ()
validateSshPubKey pubkey
	| length (lines pubkey) == 1 =
		either error return $ check $ words pubkey
	| otherwise = error "too many lines in ssh public key"
  where
	check [prefix, _key, comment] = do
		checkprefix prefix
		checkcomment comment
	check [prefix, _key] =
		checkprefix prefix
	check _ = err "wrong number of words in ssh public key"

	ok = Right ()
	err msg = Left $ unwords [msg, pubkey]

	checkprefix prefix
		| ssh == "ssh" && all isAlphaNum keytype = ok
		| otherwise = err "bad ssh public key prefix"
	  where
		(ssh, keytype) = separate (== '-') prefix

	checkcomment comment = case filter (not . safeincomment) comment of
		[] -> ok
		badstuff -> err $ "bad comment in ssh public key (contains: \"" ++ badstuff ++ "\")"
	safeincomment c = isAlphaNum c || c == '@' || c == '-' || c == '_' || c == '.'

addAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO Bool
addAuthorizedKeys rsynconly dir pubkey = boolSystem "sh"
	[ Param "-c" , Param $ addAuthorizedKeysCommand rsynconly dir pubkey ]

removeAuthorizedKeys :: Bool -> FilePath -> SshPubKey -> IO ()
removeAuthorizedKeys rsynconly dir pubkey = do
	let keyline = authorizedKeysLine rsynconly dir pubkey
	sshdir <- sshDir
	let keyfile = sshdir </> "authorized_keys"
	ls <- lines <$> readFileStrict keyfile
	writeFile keyfile $ unlines $ filter (/= keyline) ls

{- Implemented as a shell command, so it can be run on remote servers over
 - ssh.
 -
 - The ~/.ssh/git-annex-shell wrapper script is created if not already
 - present.
 -}
addAuthorizedKeysCommand :: Bool -> FilePath -> SshPubKey -> String
addAuthorizedKeysCommand rsynconly dir pubkey = intercalate "&&"
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
		, shellEscape $ authorizedKeysLine rsynconly dir pubkey
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
authorizedKeysLine rsynconly dir pubkey
	{- TODO: Locking down rsync is difficult, requiring a rather
	 - long perl script. -}
	| rsynconly = pubkey
	| otherwise = limitcommand ++ pubkey
  where
	limitcommand = "command=\"GIT_ANNEX_SHELL_DIRECTORY="++shellEscape dir++" ~/.ssh/git-annex-shell\",no-agent-forwarding,no-port-forwarding,no-X11-forwarding "

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
 - ~/.ssh/*.pub, and uses them indiscriminately. But using this key
 - for a normal login to the server will force git-annex-shell to run,
 - and locks the user out. Luckily, it does not recurse into subdirectories.
 -}
setupSshKeyPair :: SshKeyPair -> SshData -> IO SshData
setupSshKeyPair sshkeypair sshdata = do
	sshdir <- sshDir
	createDirectoryIfMissing True $ parentDir $ sshdir </> sshprivkeyfile

	unlessM (doesFileExist $ sshdir </> sshprivkeyfile) $ do
		h <- fdToHandle =<<
			createFile (sshdir </> sshprivkeyfile)
				(unionFileModes ownerWriteMode ownerReadMode)
		hPutStr h (sshPrivKey sshkeypair)
		hClose h
	unlessM (doesFileExist $ sshdir </> sshpubkeyfile) $
		writeFile (sshdir </> sshpubkeyfile) (sshPubKey sshkeypair)

	setSshConfig sshdata
		[ ("IdentityFile", "~/.ssh/" ++ sshprivkeyfile) ]
  where
	sshprivkeyfile = "git-annex" </> "key." ++ mangleSshHostName sshdata
	sshpubkeyfile = sshprivkeyfile ++ ".pub"

{- Setups up a ssh config with a mangled hostname.
 - Returns a modified SshData containing the mangled hostname. -}
setSshConfig :: SshData -> [(String, String)] -> IO SshData
setSshConfig sshdata config = do
	sshdir <- sshDir
	createDirectoryIfMissing True sshdir
	let configfile = sshdir </> "config"
	unlessM (catchBoolIO $ isInfixOf mangledhost <$> readFile configfile) $
		appendFile configfile $ unlines $
			[ ""
			, "# Added automatically by git-annex"
			, "Host " ++ mangledhost
			] ++ map (\(k, v) -> "\t" ++ k ++ " " ++ v)
				(settings ++ config)
	return $ sshdata { sshHostName = T.pack mangledhost }
  where
	mangledhost = mangleSshHostName sshdata
	settings =
		[ ("Hostname", T.unpack $ sshHostName sshdata)
		, ("Port", show $ sshPort sshdata)
		]

{- This hostname is specific to a given repository on the ssh host,
 - so it is based on the real hostname, the username, and the directory.
 -}
mangleSshHostName :: SshData -> String
mangleSshHostName sshdata = "git-annex-" ++ T.unpack (sshHostName sshdata)
	++ "-" ++ filter safe extra
  where
	extra = intercalate "_" $ map T.unpack $ catMaybes
		[ sshUserName sshdata
		, Just $ sshDirectory sshdata
		]
	safe c
		| isAlphaNum c = True
		| c == '_' = True
		| otherwise = False

{- Extracts the real hostname from a mangled ssh hostname. -}
unMangleSshHostName :: String -> String
unMangleSshHostName h = case split "-" h of
	("git":"annex":rest) -> intercalate "-" (beginning rest)
	_ -> h

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
