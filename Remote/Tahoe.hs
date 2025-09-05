{- Tahoe-LAFS special remotes.
 -
 - Tahoe capabilities for accessing objects stored in the remote
 - are preserved in the remote state log.
 -
 - In order to allow multiple clones of a repository to access the same
 - tahoe repository, git-annex needs to store the introducer furl,
 - and the shared-convergence-secret. These are stored in the remote
 - configuration, when embedcreds is enabled.
 -
 - Using those creds, git-annex sets up a tahoe configuration directory in
 - ~/.tahoe-git-annex/UUID/
 -
 - Tahoe has its own encryption, so git-annex's encryption is not used.
 -
 - Copyright 2014-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Tahoe (remote) where

import qualified Data.Map as M
import Utility.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Concurrent.STM

import Annex.Common
import Types.Remote
import Types.Creds
import Types.ProposedAccepted
import Types.NumCopies
import qualified Git
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.UUID
import Annex.Content
import Logs.RemoteState
import Utility.UserInfo
import Utility.Metered
import Utility.Env
import Utility.ThreadScheduler
import Utility.Process.Transcript

import Control.Concurrent

{- The TMVar is left empty until tahoe has been verified to be running. -}
data TahoeHandle = TahoeHandle TahoeConfigDir (TMVar ())

type TahoeConfigDir = OsPath
type SharedConvergenceSecret = String
type IntroducerFurl = String
type Capability = String

remote :: RemoteType
remote = RemoteType
	{ typename = "tahoe"
	, enumerate = const (findSpecialRemotes "tahoe")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser scsField
			(FieldDesc "optional, normally a unique one is generated")
		, optionalStringParser furlField HiddenField
		]
	, setup = tahoeSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

scsField :: RemoteConfigField
scsField = Accepted "shared-convergence-secret"

furlField :: RemoteConfigField
furlField = Accepted "introducer-furl"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c expensiveRemoteCost
	hdl <- liftIO $ TahoeHandle
		<$> maybe (defaultTahoeConfigDir u)
			(return . toOsPath)
			(remoteAnnexTahoe gc)
		<*> newEmptyTMVarIO
	return $ Just $ Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store rs hdl
		, retrieveKeyFile = retrieve rs hdl
		-- Unsure about whether tahoe might sometimes write chunks
		-- out of order.
		, retrieveKeyFileInOrder = pure False
		, retrieveKeyFileCheap = Nothing
		-- Tahoe cryptographically verifies content.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = remove
		, lockContent = Just $ lockKey u rs hdl
		, checkPresent = checkKey rs hdl
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Just (getWhereisKey rs)
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = Nothing
		, readonly = False
		, appendonly = False
		, untrustworthy = False
		, availability = pure GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

tahoeSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
tahoeSetup _ mu _ c _ = do
	furl <- maybe (fromMaybe missingfurl $ M.lookup furlField c) Proposed
		<$> liftIO (getEnv "TAHOE_FURL")
	u <- maybe (liftIO genUUID) return mu
	configdir <- liftIO $ defaultTahoeConfigDir u
	scs <- liftIO $ tahoeConfigure configdir
		(fromProposedAccepted furl)
		(fromProposedAccepted <$> (M.lookup scsField c))
	pc <- either giveup return . parseRemoteConfig c =<< configParser remote c
	let c' = if embedCreds pc
		then flip M.union c $ M.fromList
			[ (furlField, furl)
			, (scsField, Proposed scs)
			]
		else c
	gitConfigSpecialRemote u c' [("tahoe", fromOsPath configdir)]
	return (c', u)
  where
	missingfurl = giveup "Set TAHOE_FURL to the introducer furl to use."

store :: RemoteStateHandle -> TahoeHandle -> Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
store rs hdl k _af o _p = sendAnnex k o noop $ \src _sz -> do
	showOutput
	parsePut <$> liftIO (readTahoe hdl "put" [File (fromOsPath src)]) >>= maybe
		(giveup "tahoe failed to store content")
		(\cap -> storeCapability rs k cap)

retrieve :: RemoteStateHandle -> TahoeHandle -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
retrieve rs hdl k _f d _p _ = do
	go =<< getCapability rs k
	-- Tahoe verifies the content it retrieves using cryptographically
	-- secure methods.
	return Verified
  where
	go Nothing = giveup "tahoe capability is not known"
	go (Just cap) = do
		showOutput
		unlessM (liftIO $ requestTahoe hdl "get" [Param cap, File (fromOsPath d)]) $
			giveup "tahoe failed to retrieve content"

remove :: Maybe SafeDropProof -> Key -> Annex ()
remove _ _ = giveup "content cannot be removed from tahoe remote"

-- Since content cannot be removed from tahoe (by git-annex),
-- nothing needs to be done to lock content there, except for checking that
-- it is actually present.
lockKey :: UUID -> RemoteStateHandle -> TahoeHandle -> Key -> (VerifiedCopy -> Annex a) -> Annex a
lockKey u rs hrl k callback = 
	ifM (checkKey rs hrl k)
		( withVerifiedCopy LockedCopy u (return (Right True)) callback
		, giveup $ "content seems to be missing from tahoe remote"
		)

checkKey :: RemoteStateHandle -> TahoeHandle -> Key -> Annex Bool
checkKey rs hdl k = go =<< getCapability rs k
  where
	go Nothing = return False
	go (Just cap) = liftIO $ do
		v <- parseCheck <$> readTahoe hdl "check"
			[ Param "--raw"
			, Param cap
			]
		either giveup return v

defaultTahoeConfigDir :: UUID -> IO TahoeConfigDir
defaultTahoeConfigDir u = do
	h <- myHomeDir 
	return $ toOsPath h </> literalOsPath ".tahoe-git-annex" </> fromUUID u

tahoeConfigure :: TahoeConfigDir -> IntroducerFurl -> Maybe SharedConvergenceSecret -> IO SharedConvergenceSecret
tahoeConfigure configdir furl mscs = do
	unlessM (createClient configdir furl) $
		giveup "tahoe create-client failed"
	maybe noop (writeSharedConvergenceSecret configdir) mscs
	startTahoeDaemon configdir
	getSharedConvergenceSecret configdir

createClient :: TahoeConfigDir -> IntroducerFurl -> IO Bool
createClient configdir furl = do
	createDirectoryIfMissing True $ parentDir configdir
	boolTahoe configdir "create-client"
		[ Param "--nickname", Param "git-annex"
		, Param "--introducer", Param furl
		]

writeSharedConvergenceSecret :: TahoeConfigDir -> SharedConvergenceSecret -> IO ()
writeSharedConvergenceSecret configdir scs = 
	writeFileString (convergenceFile configdir)
		(unlines [scs])

{- The tahoe daemon writes the convergenceFile shortly after it starts
 - (it does not need to connect to the network). So, try repeatedly to read
 - the file, for up to 1 minute. To avoid reading a partially written
 - file, look for the newline after the value. -}
getSharedConvergenceSecret :: TahoeConfigDir -> IO SharedConvergenceSecret
getSharedConvergenceSecret configdir = go (60 :: Int)
  where
	f = convergenceFile configdir
	go n
		| n == 0 = giveup $ "tahoe did not write " ++ fromOsPath f ++ " after 1 minute. Perhaps the daemon failed to start?"
		| otherwise = do
			v <- catchMaybeIO (readFileString f)
			case v of
				Just s | "\n" `isSuffixOf` s || "\r" `isSuffixOf` s ->
					return $ takeWhile (`notElem` ("\n\r" :: String)) s
				_ -> do
					threadDelaySeconds (Seconds 1)
					go (n - 1)

convergenceFile :: TahoeConfigDir -> OsPath
convergenceFile configdir = 
	configdir </> literalOsPath "private" </> literalOsPath "convergence"

{- tahoe run stays in the foreground, but behaves as a daemon, servicing
 - requests. Attempting to start a second tahoe run process will fail. So,
 - in order to support multiple git-annex processes, it is run in the
 - background, and left running on exit.
 -
 - It can take a while for tahoe to begin accepting connections.
 - This function waits for it to get ready.
 -}
startTahoeDaemon :: TahoeConfigDir -> IO ()
startTahoeDaemon configdir = withNullHandle $ \nullh -> do
	let ps = tahoeParams configdir "run" [Param "--allow-stdin-close"]
	let p = (proc "tahoe" $ toCommand ps)
		{ std_in = UseHandle nullh
		, std_out = UseHandle nullh
		, std_err = UseHandle nullh
		}
	void $ forkIO $ void $ createProcess p
	waitready (5 :: Int)
	hClose nullh
  where
	waitready n
		| n <= 0 = giveup "The tahoe run process is not responding to requests. Waited 5 seconds for it to start."
		| otherwise = do
			-- Need something that will always succeed
			-- once the server has started and is accepting
			-- requests, and this seems to do the trick.
			let ps = tahoeParams configdir "check"
				[ Param "--raw", Param "URI:LIT:"]
			(_, ok) <- processTranscript "tahoe" 
				(toCommand ps)
				Nothing
			if ok
				then return ()
				else do
					threadDelaySeconds (Seconds 1)
					waitready (pred n)

{- Ensures that tahoe has been started, before running an action
 - that uses it. -}
withTahoeConfigDir :: TahoeHandle -> (TahoeConfigDir -> IO a) -> IO a
withTahoeConfigDir (TahoeHandle configdir v) a = go =<< atomically needsstart
  where
	go True = do
		startTahoeDaemon configdir
		a configdir
	go False = a configdir
	needsstart = ifM (isEmptyTMVar v)
		( do
			putTMVar v ()
			return True
		, return False
		)

boolTahoe :: TahoeConfigDir -> String -> [CommandParam] -> IO Bool
boolTahoe configdir command params = boolSystem "tahoe" $
	tahoeParams configdir command params

{- Runs a tahoe command that requests the daemon do something. -}
requestTahoe :: TahoeHandle -> String -> [CommandParam] -> IO Bool
requestTahoe hdl command params = withTahoeConfigDir hdl $ \configdir ->
	boolTahoe configdir command params	

{- Runs a tahoe command that requests the daemon output something. -}
readTahoe :: TahoeHandle -> String -> [CommandParam] -> IO String
readTahoe hdl command params = withTahoeConfigDir hdl $ \configdir ->
	catchDefaultIO "" $
		readProcess "tahoe" $ toCommand $
			tahoeParams configdir command params

tahoeParams :: TahoeConfigDir -> String -> [CommandParam] -> [CommandParam]
tahoeParams configdir command params = 
	Param "-d" : File (fromOsPath configdir) : Param command : params

storeCapability :: RemoteStateHandle -> Key -> Capability -> Annex ()
storeCapability rs k cap = setRemoteState rs k cap

getCapability :: RemoteStateHandle -> Key -> Annex (Maybe Capability)
getCapability rs k = getRemoteState rs k

getWhereisKey :: RemoteStateHandle -> Key -> Annex [String]
getWhereisKey rs k = disp <$> getCapability rs k
  where
	disp Nothing = []
	disp (Just c) = [c]

{- tahoe put outputs a single line, containing the capability. -}
parsePut :: String -> Maybe Capability
parsePut s = case lines s of
	[cap] | "URI" `isPrefixOf` cap -> Just cap
	_ -> Nothing

{- tahoe check --raw outputs a json document.
 - Its contents will vary (for LIT capabilities, it lacks most info),
 - but should always contain a results object with a healthy value
 - that's true or false.
 -}
parseCheck :: String -> Either String Bool
parseCheck s = maybe parseerror (Right . healthy . results) (decode $ fromString s)
  where
	parseerror
		| null s = Left "tahoe check failed to run"
		| otherwise = Left "unable to parse tahoe check output"

data CheckRet = CheckRet { results :: Results }
data Results = Results { healthy :: Bool }

instance FromJSON CheckRet where
	parseJSON (Object v) = CheckRet
		<$> v .: "results"
	parseJSON _ = mzero

instance FromJSON Results where
	parseJSON (Object v) = Results
		<$> v .: "healthy"
	parseJSON _ = mzero
