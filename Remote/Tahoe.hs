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
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Tahoe (remote) where

import qualified Data.Map as M
import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Concurrent.STM

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Annex.UUID
import Annex.Content
import Logs.RemoteState
import Utility.UserInfo
import Utility.Metered
import Utility.Env
import Utility.ThreadScheduler

{- The TMVar is left empty until tahoe has been verified to be running. -}
data TahoeHandle = TahoeHandle TahoeConfigDir (TMVar ())

type TahoeConfigDir = FilePath
type SharedConvergenceSecret = String
type IntroducerFurl = String
type Capability = String

remote :: RemoteType
remote = RemoteType {
	typename = "tahoe",
	enumerate = const (findSpecialRemotes "tahoe"),
	generate = gen,
	setup = tahoeSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	hdl <- liftIO $ TahoeHandle
		<$> maybe (defaultTahoeConfigDir u) return (remoteAnnexTahoe gc)
		<*> newEmptyTMVarIO
	return $ Just $ Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store u hdl
		, retrieveKeyFile = retrieve u hdl
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = remove
		, lockContent = Nothing
		, checkPresent = checkKey u hdl
		, checkPresentCheap = False
		, whereisKey = Just (getWhereisKey u)
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, repo = r
		, gitconfig = gc
		, localpath = Nothing
		, readonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing
		, checkUrl = Nothing
		}

tahoeSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
tahoeSetup mu _ c = do
	furl <- fromMaybe (fromMaybe missingfurl $ M.lookup furlk c)
		<$> liftIO (getEnv "TAHOE_FURL")
	u <- maybe (liftIO genUUID) return mu
	configdir <- liftIO $ defaultTahoeConfigDir u
	scs <- liftIO $ tahoeConfigure configdir furl (M.lookup scsk c)
	let c' = if M.lookup "embedcreds" c == Just "yes"
		then flip M.union c $ M.fromList
			[ (furlk, furl)
			, (scsk, scs)
			]
		else c
	gitConfigSpecialRemote u c' "tahoe" configdir
	return (c', u)
  where
	scsk = "shared-convergence-secret"
	furlk = "introducer-furl"
	missingfurl = error "Set TAHOE_FURL to the introducer furl to use."

store :: UUID -> TahoeHandle -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store u hdl k _f _p = sendAnnex k noop $ \src ->
	parsePut <$> liftIO (readTahoe hdl "put" [File src]) >>= maybe
		(return False)
		(\cap -> storeCapability u k cap >> return True)

retrieve :: UUID -> TahoeHandle -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
retrieve u hdl k _f d _p = unVerified $ go =<< getCapability u k
  where
	go Nothing = return False
	go (Just cap) = liftIO $ requestTahoe hdl "get" [Param cap, File d]

remove :: Key -> Annex Bool
remove _k = do
	warning "content cannot be removed from tahoe remote"
	return False

checkKey :: UUID -> TahoeHandle -> Key -> Annex Bool
checkKey u hdl k = go =<< getCapability u k
  where
	go Nothing = return False
	go (Just cap) = liftIO $ do
		v <- parseCheck <$> readTahoe hdl "check"
			[ Param "--raw"
			, Param cap
			]
		either error return v

defaultTahoeConfigDir :: UUID -> IO TahoeConfigDir
defaultTahoeConfigDir u = do
	h <- myHomeDir 
	return $ h </> ".tahoe-git-annex" </> fromUUID u

tahoeConfigure :: TahoeConfigDir -> IntroducerFurl -> Maybe SharedConvergenceSecret -> IO SharedConvergenceSecret
tahoeConfigure configdir furl mscs = do
	unlessM (createClient configdir furl) $
		error "tahoe create-client failed"
	maybe noop (writeSharedConvergenceSecret configdir) mscs
	startTahoeDaemon configdir
	getSharedConvergenceSecret configdir

createClient :: TahoeConfigDir -> IntroducerFurl -> IO Bool
createClient configdir furl = do
	createDirectoryIfMissing True (parentDir configdir)
	boolTahoe configdir "create-client"
		[ Param "--nickname", Param "git-annex"
		, Param "--introducer", Param furl
		]

writeSharedConvergenceSecret :: TahoeConfigDir -> SharedConvergenceSecret -> IO ()
writeSharedConvergenceSecret configdir scs = 
	writeFile (convergenceFile configdir) (unlines [scs])

{- The tahoe daemon writes the convergenceFile shortly after it starts
 - (it does not need to connect to the network). So, try repeatedly to read
 - the file, for up to 1 minute. To avoid reading a partially written
 - file, look for the newline after the value. -}
getSharedConvergenceSecret :: TahoeConfigDir -> IO SharedConvergenceSecret
getSharedConvergenceSecret configdir = go (60 :: Int)
  where
	f = convergenceFile configdir
	go n
		| n == 0 = error $ "tahoe did not write " ++ f ++ " after 1 minute. Perhaps the daemon failed to start?"
		| otherwise = do
			v <- catchMaybeIO (readFile f)
			case v of
				Just s | "\n" `isSuffixOf` s || "\r" `isSuffixOf` s ->
					return $ takeWhile (`notElem` ("\n\r" :: String)) s
				_ -> do
					threadDelaySeconds (Seconds 1)
					go (n - 1)

convergenceFile :: TahoeConfigDir -> FilePath
convergenceFile configdir = configdir </> "private" </> "convergence"

startTahoeDaemon :: TahoeConfigDir -> IO ()
startTahoeDaemon configdir = void $ boolTahoe configdir "start" []

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
	Param "-d" : File configdir : Param command : params

storeCapability :: UUID -> Key -> Capability -> Annex ()
storeCapability u k cap = setRemoteState u k cap

getCapability :: UUID -> Key -> Annex (Maybe Capability)
getCapability u k = getRemoteState u k

getWhereisKey :: UUID -> Key -> Annex [String]
getWhereisKey u k = disp <$> getCapability u k
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
