{- Remote on Android device accessed using adb.
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Adb (remote) where

import qualified Data.Map as M

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.Export
import Annex.UUID

-- | Each Android device has a serial number.
newtype AndroidSerial = AndroidSerial { fromAndroidSerial :: String }
	deriving (Show, Eq)

-- | A location on an Android device. 
newtype AndroidPath = AndroidPath { fromAndroidPath :: FilePath }

remote :: RemoteType
remote = RemoteType
	{ typename = "adb"
	, enumerate = const (findSpecialRemotes "adb")
	, generate = gen
	, setup = adbSetup
	, exportSupported = exportUnsupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	let this = Remote
		{ uuid = u
		-- adb operates over USB or wifi, so is not as cheap
		-- as local, but not too expensive
		, cost = semiExpensiveRemoteCost
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, repo = r
		, gitconfig = gc
		, localpath = Nothing
		, remotetype = remote
		, availability = LocallyAvailable
		, readonly = False
		, mkUnavailable = return Nothing
		, getInfo = return
			[ ("androidserial", fromAndroidSerial serial)
			, ("androiddirectory", fromAndroidPath adir)
			]
		, claimUrl = Nothing
		, checkUrl = Nothing
		}
	return $ Just $ specialRemote c
		(simplyPrepare $ store serial adir)
		(simplyPrepare $ retrieve serial adir)
		(simplyPrepare $ remove serial adir)
		(simplyPrepare $ checkKey this serial adir)
		this
  where
	adir = maybe (giveup "missing androiddirectory") AndroidPath
		(remoteAnnexAndroidDirectory gc)
	serial = maybe (giveup "missing androidserial") AndroidSerial
		(remoteAnnexAndroidSerial gc)

adbSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
adbSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration
	adir <- maybe (giveup "Specify androiddirectory=") (pure . AndroidPath)
		(M.lookup "androiddirectory" c)
	serial <- getserial =<< liftIO enumerateAdbConnected

	(c', _encsetup) <- encryptionSetup c gc

	ok <- liftIO $ adbShellBool serial
		[Param "mkdir", Param "-p", File (fromAndroidPath adir)]
	unless ok $
		giveup "Creating directory on Android device failed."

	gitConfigSpecialRemote u c' 
		[ ("adb", "true")
		, ("androiddirectory", fromAndroidPath adir)
		, ("androidserial", fromAndroidSerial serial)
		]

	return (c', u)
  where
	getserial [] = giveup "adb does not list any connected android devices. Plug in an Android device, or configure adb, and try again.."
	getserial (s:[]) = return s
	getserial l = case M.lookup "androidserial" c of
		Nothing -> giveup $ unlines $
			"There are multiple connected android devices, specify which to use with androidserial="
			: map fromAndroidSerial l
		Just cs
			| AndroidSerial cs `elem` l -> return (AndroidSerial cs)
			| otherwise -> giveup $ "The device with androidserial=" ++ cs ++ " is not connected."

store :: AndroidSerial -> AndroidPath -> Storer
store serial adir = fileStorer $ \k src _p -> do
	let hashdir = fromAndroidPath $ androidHashDir adir k
	liftIO $ void $ adbShell serial [Param "mkdir", Param "-p", File hashdir]
	showOutput -- make way for adb push output
	let dest = fromAndroidPath $ androidLocation adir k
	let tmpdest = dest ++ ".tmp"
	ifM (liftIO $ boolSystem "adb" (mkAdbCommand serial [Param "push", File src, File tmpdest]))
		-- move into place atomically
		( liftIO $ adbShellBool serial [Param "mv", File tmpdest, File dest]
		, return False
		)

retrieve :: AndroidSerial -> AndroidPath -> Retriever
retrieve serial adir = fileRetriever $ \d k _p -> do
	showOutput -- make way for adb pull output
	ok <- liftIO $ boolSystem "adb" $ mkAdbCommand serial
		[ Param "pull"
		, File $ fromAndroidPath $ androidLocation adir k
		, File d
		]
	unless ok $
		giveup "adb pull failed"

remove :: AndroidSerial -> AndroidPath -> Remover
remove serial adir k = liftIO $ adbShellBool serial
	[Param "rm", Param "-f", File (fromAndroidPath loc)]
  where
	loc = androidLocation adir k

checkKey :: Remote -> AndroidSerial -> AndroidPath -> CheckPresent
checkKey r serial adir k = do
	showChecking r
	(out, st) <- liftIO $ adbShellRaw serial $ unwords
		[ "if test -e ", shellEscape (fromAndroidPath loc)
		, "; then echo y"
		, "; else echo n"
		, "; fi"
		]
	case (out, st) of
		(["y"], ExitSuccess) -> return True
		(["n"], ExitSuccess) -> return False
		_ -> giveup $ "unable to access Android device" ++ show out
  where
	loc = androidLocation adir k

androidLocation :: AndroidPath -> Key -> AndroidPath
androidLocation adir k = AndroidPath $
	fromAndroidPath (androidHashDir adir k) ++ key2file k

androidHashDir :: AndroidPath -> Key -> AndroidPath
androidHashDir adir k = AndroidPath $ 
	fromAndroidPath adir ++ "/" ++ hdir
  where
	hdir = replace [pathSeparator] "/" (hashDirLower def k)

-- | List all connected Android devices.
enumerateAdbConnected :: IO [AndroidSerial]
enumerateAdbConnected = 
	mapMaybe parse . lines <$> readProcess "adb" ["devices"]
  where
	parse l = 
		let (serial, desc) = separate (== '\t') l
		in if null desc || length serial /= 16
			then Nothing 
			else Just (AndroidSerial serial)

-- | Runs a command on the android device with the given serial number.
--
-- adb shell does not propigate the exit code of the command, so
-- it is echoed out in a trailing line, and the output is read to determine
-- it. Any stdout from the command is returned, separated into lines.
adbShell :: AndroidSerial -> [CommandParam] -> IO ([String], ExitCode)
adbShell serial cmd = adbShellRaw serial $
	unwords $ map shellEscape (toCommand cmd)

adbShellBool :: AndroidSerial -> [CommandParam] -> IO Bool
adbShellBool serial cmd = do
	(_ , ec) <- adbShell serial cmd
	return (ec == ExitSuccess)

-- | Runs a raw shell command on the android device.
-- Any necessary shellEscaping must be done by caller.
adbShellRaw :: AndroidSerial -> String -> IO ([String], ExitCode)
adbShellRaw serial cmd = processoutput <$> readProcess "adb"
	[ "-s"
	, fromAndroidSerial serial
	, "shell"
	-- The extra echo is in case cmd does not output a trailing
	-- newline after its other output.
	, cmd ++ "; echo; echo $?"
	]
  where
	processoutput s = case reverse (map trimcr (lines s)) of
		(c:"":rest) -> case readish c of
			Just 0 -> (reverse rest, ExitSuccess)
			Just n -> (reverse rest, ExitFailure n)
			Nothing -> (reverse rest, ExitFailure 1)
		ls -> (reverse ls, ExitFailure 1)
	-- For some reason, adb outputs lines with \r\n on linux,
	-- despite both linux and android being unix systems.
	trimcr = takeWhile (/= '\r')

mkAdbCommand :: AndroidSerial -> [CommandParam] -> [CommandParam]
mkAdbCommand serial cmd = [Param "-s", Param (fromAndroidSerial serial)] ++ cmd
