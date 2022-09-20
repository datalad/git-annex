{- Remote on Android device accessed using adb.
 -
 - Copyright 2018-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Adb (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import Types.Export
import Types.Import
import qualified Git
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.UUID
import Utility.Metered
import Types.ProposedAccepted
import Annex.SpecialRemote.Config
import Annex.Verify

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
			(FieldDesc "location on the Android device where the files are stored")
		, optionalStringParser androidserialField
			(FieldDesc "sometimes needed to specify which Android device to use")
		, yesNoParser ignorefinderrorField (Just False)
			(FieldDesc "ignore adb find errors")
		, yesNoParser oldandroidField (Just False)
			(FieldDesc "support old versions of android (slower)")
		]
	, setup = adbSetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = False
	}

androiddirectoryField :: RemoteConfigField
androiddirectoryField = Accepted "androiddirectory"

androidserialField :: RemoteConfigField
androidserialField = Accepted "androidserial"

ignorefinderrorField :: RemoteConfigField
ignorefinderrorField = Accepted "ignorefinderror"

oldandroidField :: RemoteConfigField
oldandroidField = Accepted "oldandroid"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	let this = Remote
		{ uuid = u
		-- adb operates over USB or wifi, so is not as cheap
		-- as local, but not too expensive
		, cost = semiExpensiveRemoteCost
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, exportActions = ExportActions
			{ storeExport = storeExportM serial adir
			, retrieveExport = retrieveExportM serial adir
			, removeExport = removeExportM serial adir
			, versionedExport = False
			, checkPresentExport = checkPresentExportM serial adir
			, removeExportDirectory = Just $ removeExportDirectoryM serial adir
			, renameExport = renameExportM serial adir
			}
		, importActions = ImportActions
			{ listImportableContents = listImportableContentsM serial adir c
			, importKey = Nothing
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
		, untrustworthy = False
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
		(store serial adir)
		(retrieve serial adir)
		(remove serial adir)
		(checkKey serial adir)
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
	serial <- getserial =<< enumerateAdbConnected
	let c' = M.insert androidserialField (Proposed (fromAndroidSerial serial)) c

	(c'', _encsetup) <- encryptionSetup c' gc

	ok <- adbShellBool serial
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
	in unlessM (store' serial dest src) $
		giveup "adb failed"

store' :: AndroidSerial -> AndroidPath -> FilePath -> Annex Bool
store' serial dest src = store'' serial dest src (return True)

store'' :: AndroidSerial -> AndroidPath -> FilePath -> Annex Bool -> Annex Bool
store'' serial dest src canoverwrite = checkAdbInPath False $ do
	let destdir = takeDirectory $ fromAndroidPath dest
	void $ adbShell serial [Param "mkdir", Param "-p", File destdir]
	showOutput -- make way for adb push output
	let tmpdest = fromAndroidPath dest ++ ".annextmp"
	ifM (liftIO $ boolSystem "adb" (mkAdbCommand serial [Param "push", File src, File tmpdest]))
		( ifM canoverwrite
			-- move into place atomically
			( adbShellBool serial [Param "mv", File tmpdest, File (fromAndroidPath dest)]
			, do
				void $ remove' serial (AndroidPath tmpdest)
				return False
			)
		, return False
		)

retrieve :: AndroidSerial -> AndroidPath -> Retriever
retrieve serial adir = fileRetriever $ \dest k _p ->
	let src = androidLocation adir k
	in retrieve' serial src (fromRawFilePath dest)

retrieve' :: AndroidSerial -> AndroidPath -> FilePath -> Annex ()
retrieve' serial src dest =
	unlessM go $
		giveup "adb pull failed"
  where
	go = checkAdbInPath False $ do
		showOutput -- make way for adb pull output
		liftIO $ boolSystem "adb" $ mkAdbCommand serial
			[ Param "pull"
			, Param "-a"
			, File $ fromAndroidPath src
			, File dest
			]

remove :: AndroidSerial -> AndroidPath -> Remover
remove serial adir k =
	unlessM (remove' serial (androidLocation adir k)) $
		giveup "adb failed"

remove' :: AndroidSerial -> AndroidPath -> Annex Bool
remove' serial aloc = adbShellBool serial
	[Param "rm", Param "-f", File (fromAndroidPath aloc)]

checkKey :: AndroidSerial -> AndroidPath -> CheckPresent
checkKey serial adir k = checkKey' serial (androidLocation adir k)

checkKey' :: AndroidSerial -> AndroidPath -> Annex Bool
checkKey' serial aloc = do
	out <- adbShellRaw serial $ unwords
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

storeExportM :: AndroidSerial -> AndroidPath -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportM serial adir src _k loc _p = 
	unlessM (store' serial dest src) $
		giveup "adb failed"
  where
	dest = androidExportLocation adir loc

retrieveExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Verification
retrieveExportM serial adir k loc dest _p = 
	verifyKeyContentIncrementally AlwaysVerify k $ \iv ->
		tailVerify iv (toRawFilePath dest) $
			retrieve' serial src dest
  where
	src = androidExportLocation adir loc

removeExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> Annex ()
removeExportM serial adir _k loc =
	unlessM (remove' serial aloc) $
		giveup "adb failed"
  where
	aloc = androidExportLocation adir loc

removeExportDirectoryM :: AndroidSerial -> AndroidPath -> ExportDirectory -> Annex ()
removeExportDirectoryM serial abase dir =
	unlessM go $
		giveup "adb failed"
  where
	go = adbShellBool serial [Param "rm", Param "-rf", File (fromAndroidPath adir)]
	adir = androidExportLocation abase (mkExportLocation (fromExportDirectory dir))

checkPresentExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM serial adir _k loc = checkKey' serial aloc
  where
	aloc = androidExportLocation adir loc

renameExportM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportM serial adir _k old new = do
	unlessM (adbShellBool serial ps) $
		giveup "adb failed"
	return (Just ())
  where
	oldloc = fromAndroidPath $ androidExportLocation adir old
	newloc = fromAndroidPath $ androidExportLocation adir new
	ps =
		[ Param "mv"
		, Param "-f"
		, File oldloc
		, File newloc
		]

listImportableContentsM :: AndroidSerial -> AndroidPath -> ParsedRemoteConfig -> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
listImportableContentsM serial adir c = adbfind >>= \case
	Just ls -> return $ Just $ ImportableContentsComplete $ 
		ImportableContents (mapMaybe mk ls) []
	Nothing -> giveup "adb find failed"
  where
	adbfind = adbShell' serial findparams
		(\s -> if oldandroid
			then s ++ ignorefinderrorsh
			else concat
				[ "set -o pipefail; "
				, s
				, "| xargs -0 stat -c " ++ shellEscape statformat ++ " --"
				, ignorefinderrorsh
				]
		)
	
	findparams =
		[ Param "find"
		-- trailing slash is needed, or android's find command
		-- won't recurse into the directory
		, File $ fromAndroidPath adir ++ "/"
		, Param "-type", Param "f"
		] ++ if oldandroid
			then 
				[ Param "-exec"
				, Param "stat"
				, Param "-c"
				, Param statformat
				, Param "{}"
				, Param ";"
				]
			else 
				[ Param "-printf"
				, Param "%p\\0"
				]

	ignorefinderror = fromMaybe False (getRemoteConfigValue ignorefinderrorField c)
	oldandroid = fromMaybe False (getRemoteConfigValue oldandroidField c)

	statformat = adbStatFormat ++ "\t%n"

	ignorefinderrorsh = if ignorefinderror then " || true" else ""

	mk ('S':'T':'\t':l) =
		let (stat, fn) = separate (== '\t') l
		    sz = fromMaybe 0 (readish (takeWhile (/= ' ') stat))
		    cid = ContentIdentifier (encodeBS stat)
		    loc = mkImportLocation $ toRawFilePath $ 
		    	Posix.makeRelative (fromAndroidPath adir) fn
		in Just (loc, (cid, sz))
	mk _ = Nothing

-- This does not guard against every possible race. As long as the adb
-- connection is resonably fast, it's probably as good as
-- git's handling of similar situations with files being modified while
-- it's updating the working tree for a merge.
retrieveExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> ExportLocation -> [ContentIdentifier] -> FilePath -> Either Key (Annex Key) -> MeterUpdate -> Annex (Key, Verification)
retrieveExportWithContentIdentifierM serial adir loc cids dest gk _p = do
	case gk of
		Right mkkey -> do
			go
			k <- mkkey
			return (k, UnVerified)
		Left k -> do
			v <- verifyKeyContentIncrementally DefaultVerify k
				(\iv -> tailVerify iv (toRawFilePath dest) go)
			return (k, v)
  where
	go = do
		retrieve' serial src dest
		getExportContentIdentifier serial adir loc >>= \case
			Right (Just currcid)
				| any (currcid ==) cids -> return ()
			_ -> giveup "the file on the android device has changed"
	src = androidExportLocation adir loc

storeExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierM serial adir src _k loc overwritablecids _p =
	-- Check if overwrite is safe before sending, because sending the
	-- file is expensive and don't want to do it unncessarily.
	ifM checkcanoverwrite
		( ifM (store'' serial dest src checkcanoverwrite)
			( getExportContentIdentifier serial adir loc >>= \case
				Right (Just cid) -> return cid
				Right Nothing -> giveup "adb failed to store file"
				Left _ -> giveup "unable to get content identifier for file stored by adb"
			, giveup "adb failed to store file"
			)
		, giveup "unsafe to overwrite file"
		)
  where
	dest = androidExportLocation adir loc
	checkcanoverwrite =
		getExportContentIdentifier serial adir loc >>= return . \case
			Right (Just cid) | cid `elem` overwritablecids -> True
			Right Nothing -> True
			_ -> False

removeExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
removeExportWithContentIdentifierM serial adir k loc removeablecids =
	getExportContentIdentifier serial adir loc >>= \case
		Right Nothing -> return ()
		Right (Just cid)
			| cid `elem` removeablecids ->
				removeExportM serial adir k loc
			| otherwise -> giveup "file on Android device is modified, cannot remove"
		Left _ -> giveup "unable to access Android device"

checkPresentExportWithContentIdentifierM :: AndroidSerial -> AndroidPath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM serial adir _k loc knowncids = 
	getExportContentIdentifier serial adir loc >>= \case
		Right (Just cid) | cid `elem` knowncids -> return True
		Right _ -> return False
		Left _ -> giveup "unable to access Android device"

androidExportLocation :: AndroidPath -> ExportLocation -> AndroidPath
androidExportLocation adir loc = AndroidPath $
	fromAndroidPath adir ++ "/" ++ fromRawFilePath (fromExportLocation loc)

-- | List all connected Android devices.
enumerateAdbConnected :: Annex [AndroidSerial]
enumerateAdbConnected = checkAdbInPath [] $ liftIO $
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
adbShell :: AndroidSerial -> [CommandParam] -> Annex (Maybe [String])
adbShell serial cmd = adbShell' serial cmd id

adbShell' :: AndroidSerial -> [CommandParam] -> (String -> String) -> Annex (Maybe [String])
adbShell' serial cmd f = adbShellRaw serial $
	f (unwords $ map shellEscape (toCommand cmd))

adbShellBool :: AndroidSerial -> [CommandParam] -> Annex Bool
adbShellBool serial cmd =
	adbShellRaw serial cmd' >>= return . \case
		Just l -> end l == ["y"]
		Nothing -> False
  where
	cmd' = "if " ++ unwords (map shellEscape (toCommand cmd))
		++ "; then echo y; else echo n; fi"

-- | Runs a raw shell command on the android device.
-- Any necessary shellEscaping must be done by caller.
adbShellRaw :: AndroidSerial -> String -> Annex (Maybe [String])
adbShellRaw serial cmd = checkAdbInPath Nothing $ liftIO $ catchMaybeIO $ 
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

checkAdbInPath :: a -> Annex a -> Annex a
checkAdbInPath d a = ifM (isJust <$> liftIO (searchPath "adb"))
	( a
	, do
		warning "adb command not found in PATH. Install it to use this remote."
		return d
	)

mkAdbCommand :: AndroidSerial -> [CommandParam] -> [CommandParam]
mkAdbCommand serial cmd = [Param "-s", Param (fromAndroidSerial serial)] ++ cmd

-- Gets the current content identifier for a file on the android device.
-- If the file is not present, returns Right Nothing
getExportContentIdentifier :: AndroidSerial -> AndroidPath -> ExportLocation -> Annex (Either ExitCode (Maybe ContentIdentifier))
getExportContentIdentifier serial adir loc = do
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
			ContentIdentifier (encodeBS stat)
		_ -> Left (ExitFailure 1)
  where
	aloc = fromAndroidPath $ androidExportLocation adir loc

-- Includes size, modificiation time, and inode.
-- Device not included because the adb interface ensures we're talking to
-- the same android device.
adbStatFormat :: String
adbStatFormat = "ST\t%s %Y %i"
