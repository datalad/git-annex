{- Remote on Android device accessed using adb.
 -
 - Copyright 2018-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Adb (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import Types.Export
import Types.Import
import qualified Git
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import Annex.UUID
import Utility.Metered
import Types.ProposedAccepted
import Annex.SpecialRemote.Config

import qualified Data.Map as M
import qualified System.FilePath.Posix as Posix

-- | Each Android device has a serial number.
newtype AndroidSerial = AndroidSerial { fromAndroidSerial :: String }
	deriving (Show, Eq)

-- | A location on an Android device. 
newtype AndroidPath = AndroidPath { fromAndroidPath :: FilePath }

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "adb"
	, enumerate = const (findSpecialRemotes "adb")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser androiddirectoryField
		, optionalStringParser androidserialField
		]
	, setup = adbSetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	}

androiddirectoryField :: RemoteConfigField
androiddirectoryField = Accepted "androiddirectory"

androidserialField :: RemoteConfigField
androidserialField = Accepted "androidserial"

gen :: Git.Repo -> UUID -> ParsedRemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u c gc rs = do
	let this = Remote
		{ uuid = u
		-- adb operates over USB or wifi, so is not as cheap
		-- as local, but not too expensive
		, cost = semiExpensiveRemoteCost
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, exportActions = ExportActions
			{ storeExport = storeExportM serial adir
			, retrieveExport = retrieveExportM serial adir
			, removeExport = removeExportM serial adir
			, checkPresentExport = checkPresentExportM this serial adir
			, removeExportDirectory = Just $ removeExportDirectoryM serial adir
			, renameExport = renameExportM serial adir
			}
		, importActions = ImportActions
			{ listImportableContents = listImportableContentsM serial adir
			, retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierM serial adir
			, storeExportWithContentIdentifier = storeExportWithContentIdentifierM serial adir
			, removeExportWithContentIdentifier = removeExportWithContentIdentifierM serial adir
			, removeExportDirectoryWhenEmpty = Nothing
			, checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierM serial adir
			}
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = Nothing
		, remotetype = remote
		, availability = LocallyAvailable
		, readonly = False
		, appendonly = False
		, mkUnavailable = return Nothing
		, getInfo = return
			[ ("androidserial", fromAndroidSerial serial)
			, ("androiddirectory", fromAndroidPath adir)
			]
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
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
	adir <- maybe
		(giveup "Specify androiddirectory=")
		(pure . AndroidPath . fromProposedAccepted)
		(M.lookup androiddirectoryField c)
	serial <- getserial =<< liftIO enumerateAdbConnected
	let c' = M.insert androidserialField (Proposed (fromAndroidSerial serial)) c

	(c'', _encsetup) <- encryptionSetup c' gc

	ok <- liftIO $ adbShellBool serial
		[Param "mkdir", Param "-p", File (fromAndroidPath adir)]
	unless ok $
		giveup "Creating directory on Android device failed."

	gitConfigSpecialRemote u c''
		[ ("adb", "true")
		, ("androiddirectory", fromAndroidPath adir)
		, ("androidserial", fromAndroidSerial serial)
		]

	return (c'', u)
  where
	getserial [] = giveup "adb does not list any connected android devices. Plug in an Android device, or configure adb, and try again.."
	getserial l = case fromProposedAccepted <$> M.lookup androidserialField c of
		Nothing -> case l of
			(s:[]) -> return s
			_ -> giveup $ unlines $
				"There are multiple connected android devices, specify which to use with androidserial="
				: map fromAndroidSerial l
		Just cs
			| AndroidSerial cs `elem` l -> return (AndroidSerial cs)
			| otherwise -> giveup $ "The device with androidserial=" ++ cs ++ " is not connected."

store :: AndroidSerial -> AndroidPath -> Storer
store serial adir = fileStorer $ \k src _p -> 
	let dest = androidLocation adir k
	in store' serial dest src

store' :: AndroidSerial -> AndroidPath -> FilePath -> Annex Bool
store' serial dest src = store'' serial dest src (return True)

store'' :: AndroidSerial -> AndroidPath -> FilePath -> Annex Bool -> Annex Bool
store'' serial dest src canoverwrite = do
	let destdir = takeDirectory $ fromAndroidPath dest
	liftIO $ void $ adbShell serial [Param "mkdir", Param "-p", File destdir]
	showOutput -- make way for adb push output
	let tmpdest = fromAndroidPath dest ++ ".annextmp"
	ifM (liftIO $ boolSystem "adb" (mkAdbCommand serial [Param "push", File src, File tmpdest]))
		( ifM canoverwrite
			-- move into place atomically
			( liftIO $ adbShellBool serial [Param "mv", File tmpdest, File (fromAndroidPath dest)]
			, do
				void $ remove' serial (AndroidPath tmpdest)
				return False
			)
		, return False
		)

retrieve :: AndroidSerial -> AndroidPath -> Retriever
retrieve serial adir = fileRetriever $ \dest k _p ->
	let src = androidLocation adir k
	in unlessM (retrieve' serial src dest) $
		giveup "adb pull failed"

retrieve' :: AndroidSerial -> AndroidPath -> FilePath -> Annex Bool
retrieve' serial src dest = do
	showOutput -- make way for adb pull output
	liftIO $ boolSystem "adb" $ mkAdbCommand serial
		[ Param "pull"
		, File $ fromAndroidPath src
		, File dest
		]

remove :: AndroidSerial -> AndroidPath -> Remover
remove serial adir k = remove' serial (androidLocation adir k)

remove' :: AndroidSerial -> AndroidPath -> Annex Bool
remove' serial aloc = liftIO $ adbShellBool serial
	[Param "rm", Param "-f", File (fromAndroidPath aloc)]

checkKey :: Remote -> AndroidSerial -> AndroidPath -> CheckPresent
checkKey r serial adir k = checkKey' r serial (androidLocation adir k)

checkKey' :: Remote -> AndroidSerial -> AndroidPath -> Annex Bool
checkKey' r serial aloc = do
	showChecking r
	out <- liftIO $ adbShellRaw serial $ unwords
		[ "if test -e ", shellEscape (fromAndroidPath aloc)
		, "; then echo y"
		, "; else echo n"
		, "; fi"
		]
	case out of
		Just ["y"] -> return True
		Just ["n"] -> return False
		_ -> giveup "unable to access Android device"

androidLocation :: AndroidPath -> Key -> AndroidPath
androidLocation adir k = AndroidPath $
	fromAndroidPath (androidHashDir adir k) ++ serializeKey k

androidHashDir :: AndroidPath -> Key -> AndroidPath
androidHashDir adir k = AndroidPath $ 
	fromAndroidPath adir ++ "/" ++ hdir
  where
	hdir = replace [pathSeparator] "/" (fromRawFilePath (hashDirLower def k))

storeExportM :: AndroidSerial -> AndroidPath -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool 
storeExportM serial adir src _k loc _p = store' serial dest src
  where
	dest = androidExportLocation adir loc

retrieveExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportM serial adir _k loc dest _p = retrieve' serial src dest
  where
	src = androidExportLocation adir loc

removeExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> Annex Bool
removeExportM serial adir _k loc = remove' serial aloc
  where
	aloc = androidExportLocation adir loc

removeExportDirectoryM :: AndroidSerial -> AndroidPath -> ExportDirectory -> Annex Bool
removeExportDirectoryM serial abase dir = liftIO $ adbShellBool serial
	[Param "rm", Param "-rf", File (fromAndroidPath adir)]
  where
	adir = androidExportLocation abase (mkExportLocation (fromExportDirectory dir))

checkPresentExportM :: Remote -> AndroidSerial -> AndroidPath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM r serial adir _k loc = checkKey' r serial aloc
  where
	aloc = androidExportLocation adir loc

renameExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe Bool)
renameExportM serial adir _k old new = liftIO $ Just <$> 
	adbShellBool serial [Param "mv", Param "-f", File oldloc, File newloc]
  where
	oldloc = fromAndroidPath $ androidExportLocation adir old
	newloc = fromAndroidPath $ androidExportLocation adir new

listImportableContentsM :: AndroidSerial -> AndroidPath -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContentsM serial adir = liftIO $
	process <$> adbShell serial
		[ Param "find"
		-- trailing slash is needed, or android's find command
		-- won't recurse into the directory
		, File $ fromAndroidPath adir ++ "/"
		, Param "-type", Param "f"
		, Param "-exec", Param "stat"
		, Param "-c", Param statformat
		, Param "{}", Param "+"
		]
  where
	process Nothing = Nothing
	process (Just ls) = Just $ ImportableContents (mapMaybe mk ls) []

	statformat = adbStatFormat ++ "\t%n"

	mk ('S':'T':'\t':l) =
		let (stat, fn) = separate (== '\t') l
		    sz = fromMaybe 0 (readish (takeWhile (/= ' ') stat))
		    cid = ContentIdentifier (encodeBS' stat)
		    loc = mkImportLocation $ toRawFilePath $ 
		    	Posix.makeRelative (fromAndroidPath adir) fn
		in Just (loc, (cid, sz))
	mk _ = Nothing

-- This does not guard against every possible race. As long as the adb
-- connection is resonably fast, it's probably as good as
-- git's handling of similar situations with files being modified while
-- it's updating the working tree for a merge.
retrieveExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> ExportLocation -> ContentIdentifier -> FilePath -> Annex (Maybe Key) -> MeterUpdate -> Annex (Maybe Key)
retrieveExportWithContentIdentifierM serial adir loc cid dest mkkey _p = catchDefaultIO Nothing $
	ifM (retrieve' serial src dest)
		( do
			k <- mkkey
			currcid <- liftIO $ getExportContentIdentifier serial adir loc
			return $ if currcid == Right (Just cid)
				then k
				else Nothing
		, return Nothing
		)
  where
	src = androidExportLocation adir loc

storeExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex (Either String ContentIdentifier)
storeExportWithContentIdentifierM serial adir src _k loc overwritablecids _p =
	-- Check if overwrite is safe before sending, because sending the
	-- file is expensive and don't want to do it unncessarily.
	ifM checkcanoverwrite
		( ifM (store'' serial dest src checkcanoverwrite)
			( liftIO $ getExportContentIdentifier serial adir loc >>= return . \case
				Right (Just cid) -> Right cid
				Right Nothing -> Left "adb failed to store file"
				Left _ -> Left "unable to get content identifier for file stored on adtb"
			, return $ Left "adb failed to store file"
			)
		, return $ Left "unsafe to overwrite file"
		)
  where
	dest = androidExportLocation adir loc
	checkcanoverwrite = liftIO $
		getExportContentIdentifier serial adir loc >>= return . \case
			Right (Just cid) | cid `elem` overwritablecids -> True
			Right Nothing -> True
			_ -> False

removeExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
removeExportWithContentIdentifierM serial adir k loc removeablecids = catchBoolIO $
	liftIO (getExportContentIdentifier serial adir loc) >>= \case
		Right Nothing -> return True
		Right (Just cid) | cid `elem` removeablecids ->
			removeExportM serial adir k loc
		_ -> return False

checkPresentExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM serial adir _k loc knowncids = 
	liftIO $ getExportContentIdentifier serial adir loc >>= \case
		Right (Just cid) | cid `elem` knowncids -> return True
		Right _ -> return False
		Left _ -> giveup "unable to access Android device"

androidExportLocation :: AndroidPath -> ExportLocation -> AndroidPath
androidExportLocation adir loc = AndroidPath $
	fromAndroidPath adir ++ "/" ++ fromRawFilePath (fromExportLocation loc)

-- | List all connected Android devices.
enumerateAdbConnected :: IO [AndroidSerial]
enumerateAdbConnected = 
	mapMaybe parse . lines <$> readProcess "adb" ["devices"]
  where
	parse l = 
		let (serial, desc) = separate (== '\t') l
		in if null desc || length serial < 4
			then Nothing 
			else Just (AndroidSerial serial)

-- | Runs a command on the android device with the given serial number.
--
-- Any stdout from the command is returned, separated into lines.
adbShell :: AndroidSerial -> [CommandParam] -> IO (Maybe [String])
adbShell serial cmd = adbShellRaw serial $
	unwords $ map shellEscape (toCommand cmd)

adbShellBool :: AndroidSerial -> [CommandParam] -> IO Bool
adbShellBool serial cmd =
	adbShellRaw serial cmd' >>= return . \case
		Just l -> end l == ["y"]
		Nothing -> False
  where
	cmd' = "if " ++ unwords (map shellEscape (toCommand cmd))
		++ "; then echo y; else echo n; fi"

-- | Runs a raw shell command on the android device.
-- Any necessary shellEscaping must be done by caller.
adbShellRaw :: AndroidSerial -> String -> IO (Maybe [String])
adbShellRaw serial cmd = catchMaybeIO $ 
	processoutput <$> readProcess "adb"
		[ "-s"
		, fromAndroidSerial serial
		, "shell"
		, cmd
		]
  where
	processoutput s = map trimcr (lines s)
	-- For some reason, adb outputs lines with \r\n on linux,
	-- despite both linux and android being unix systems.
	trimcr = takeWhile (/= '\r')

mkAdbCommand :: AndroidSerial -> [CommandParam] -> [CommandParam]
mkAdbCommand serial cmd = [Param "-s", Param (fromAndroidSerial serial)] ++ cmd

-- Gets the current content identifier for a file on the android device.
-- If the file is not present, returns Right Nothing
getExportContentIdentifier :: AndroidSerial -> AndroidPath -> ExportLocation -> IO (Either ExitCode (Maybe ContentIdentifier))
getExportContentIdentifier serial adir loc = liftIO $ do
	ls <- adbShellRaw serial $ unwords
		[ "if test -e ", shellEscape aloc
		, "; then stat -c"
		, shellEscape adbStatFormat
		, shellEscape aloc
		, "; else echo n"
		, "; fi"
		]
	return $ case ls of
		Just ["n"] -> Right Nothing
		Just (('S':'T':'\t':stat):[]) -> Right $ Just $
			ContentIdentifier (encodeBS' stat)
		_ -> Left (ExitFailure 1)
  where
	aloc = fromAndroidPath $ androidExportLocation adir loc

-- Includes size, modificiation time, and inode.
-- Device not included because the adb interface ensures we're talking to
-- the same android device.
adbStatFormat :: String
adbStatFormat = "ST\t%s %Y %i"
