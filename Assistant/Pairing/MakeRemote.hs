{- git-annex assistant pairing remote creation
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing.MakeRemote where

import Assistant.Common
import Assistant.Ssh
import Assistant.Pairing
import Assistant.Pairing.Network
import Assistant.MakeRemote
import Assistant.Sync
import Config.Cost
import Config
import qualified Types.Remote as Remote

import Network.Socket
import qualified Data.Text as T

{- Authorized keys are set up before pairing is complete, so that the other
 - side can immediately begin syncing. -}
setupAuthorizedKeys :: PairMsg -> FilePath -> IO ()
setupAuthorizedKeys msg repodir = case validateSshPubKey $ remoteSshPubKey $ pairMsgData msg of
	Left err -> error err
	Right pubkey -> do
		absdir <- absPath repodir
		unlessM (liftIO $ addAuthorizedKeys True absdir pubkey) $
			error "failed setting up ssh authorized keys"

{- When local pairing is complete, this is used to set up the remote for
 - the host we paired with. -}
finishedLocalPairing :: PairMsg -> SshKeyPair -> Assistant ()
finishedLocalPairing msg keypair = do
	sshdata <- liftIO $ installSshKeyPair keypair =<< pairMsgToSshData msg
	{- Ensure that we know the ssh host key for the host we paired with.
	 - If we don't, ssh over to get it. -}
	liftIO $ unlessM (knownHost $ sshHostName sshdata) $
		void $ sshTranscript
			[ sshOpt "StrictHostKeyChecking" "no"
			, sshOpt "NumberOfPasswordPrompts" "0"
			, "-n"
			, genSshHost (sshHostName sshdata) (sshUserName sshdata)
			, "git-annex-shell -c configlist " ++ T.unpack (sshDirectory sshdata)
			]
			Nothing
	r <- liftAnnex $ addRemote $ makeSshRemote sshdata
	liftAnnex $ setRemoteCost (Remote.repo r) semiExpensiveRemoteCost
	syncRemote r

{- Mostly a straightforward conversion.  Except:
 -  * Determine the best hostname to use to contact the host.
 -  * Strip leading ~/ from the directory name.
 -}
pairMsgToSshData :: PairMsg -> IO SshData
pairMsgToSshData msg = do
	let d = pairMsgData msg
	hostname <- liftIO $ bestHostName msg
	let dir = case remoteDirectory d of
		('~':'/':v) -> v
		v -> v
	return SshData
		{ sshHostName = T.pack hostname
		, sshUserName = Just (T.pack $ remoteUserName d)
		, sshDirectory = T.pack dir
		, sshRepoName = genSshRepoName hostname dir
		, sshPort = 22
		, needsPubKey = True
		, sshCapabilities = [GitAnnexShellCapable, GitCapable, RsyncCapable]
		, sshRepoUrl = Nothing
		}

{- Finds the best hostname to use for the host that sent the PairMsg.
 -
 - If remoteHostName is set, tries to use a .local address based on it.
 - That's the most robust, if this system supports .local.
 - Otherwise, looks up the hostname in the DNS for the remoteAddress,
 - if any. May fall back to remoteAddress if there's no DNS. Ugh. -}
bestHostName :: PairMsg -> IO HostName
bestHostName msg = case remoteHostName $ pairMsgData msg of
	Just h -> do
		let localname = h ++ ".local"
		addrs <- catchDefaultIO [] $
			getAddrInfo Nothing (Just localname) Nothing
		maybe fallback (const $ return localname) (headMaybe addrs)
	Nothing -> fallback
  where
	fallback = do
		let a = pairMsgAddr msg
		let sockaddr = case a of
			IPv4Addr addr -> SockAddrInet (fromInteger 0) addr
			IPv6Addr addr -> SockAddrInet6 (fromInteger 0) 0 addr 0
		fromMaybe (showAddr a)
			<$> catchDefaultIO Nothing
				(fst <$> getNameInfo [] True False sockaddr)
