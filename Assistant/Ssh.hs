{- git-annex assistant ssh utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Ssh where

import Common
import Utility.TempFile

import Data.Text (Text)
import qualified Data.Text as T
import qualified Control.Exception as E
import System.Process (CreateProcess(..))
import Control.Concurrent

data SshData = SshData
	{ sshHostName :: Text
	, sshUserName :: Maybe Text
	, sshDirectory :: Text
	, sshRepoName :: String
	, needsPubKey :: Bool
	, rsyncOnly :: Bool
	}
	deriving (Read, Show, Eq)

data SshKeyPair = SshKeyPair
	{ sshPubKey :: String
	, sshPrivKey :: String
	}

{- ssh -ofoo=bar command-line option -}
sshOpt :: String -> String -> String
sshOpt k v = concat ["-o", k, "=", v]

sshDir :: IO FilePath
sshDir = do
	home <- myHomeDir
	return $ home </> ".ssh"

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

{- Implemented as a shell command, so it can be run on remote servers over
 - ssh. -}
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
