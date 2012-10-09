{- git-annex assistant pairing remote creation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing.MakeRemote where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.Ssh
import Assistant.Pairing
import Assistant.Pairing.Network
import Assistant.MakeRemote

import Network.Socket
import qualified Data.Text as T

{- Authorized keys are set up before pairing is complete, so that the other
 - side can immediately begin syncing. -}
setupAuthorizedKeys :: PairMsg -> IO ()
setupAuthorizedKeys msg = do
	validateSshPubKey pubkey
	unlessM (liftIO $ addAuthorizedKeys False pubkey) $
		error "failed setting up ssh authorized keys"
	where
		pubkey = remoteSshPubKey $ pairMsgData msg

{- When pairing is complete, this is used to set up the remote for the host
 - we paired with. -}
finishedPairing :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> PairMsg -> SshKeyPair -> IO ()
finishedPairing st dstatus scanremotes msg keypair = do
	sshdata <- setupSshKeyPair keypair =<< pairMsgToSshData msg
	{- Ensure that we know
	 - the ssh host key for the host we paired with.
	 - If we don't, ssh over to get it. -}
	unlessM (knownHost $ sshHostName sshdata) $
		void $ sshTranscript
			[ sshOpt "StrictHostKeyChecking" "no"
			, sshOpt "NumberOfPasswordPrompts" "0"
			, "-n"
			, genSshHost (sshHostName sshdata) (sshUserName sshdata)
			, "git-annex-shell -c configlist " ++ T.unpack (sshDirectory sshdata)
			]
			""
	void $ makeSshRemote st dstatus scanremotes False sshdata

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
		, needsPubKey = True
		, rsyncOnly = False
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
				IPv4Addr addr -> SockAddrInet (PortNum 0) addr
				IPv6Addr addr -> SockAddrInet6 (PortNum 0) 0 addr 0
			fromMaybe (showAddr a)
				<$> catchDefaultIO Nothing
					(fst <$> getNameInfo [] True False sockaddr)
