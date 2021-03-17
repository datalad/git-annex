{- Amazon Glacier remotes.
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Glacier (remote, jobList, checkSaneGlacierCommand) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Annex.Common
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import qualified Remote.Helper.AWS as AWS
import Creds
import Utility.Metered
import Annex.UUID
import Utility.Env
import Types.ProposedAccepted

type Vault = String
type Archive = FilePath

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "glacier"
	, enumerate = const (findSpecialRemotes "glacier")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser datacenterField
			(FieldDesc "S3 datacenter to use")
		, optionalStringParser vaultField
			(FieldDesc "name to use for vault")
		, optionalStringParser fileprefixField
			(FieldDesc "prefix to add to filenames in the vault")
		, optionalStringParser AWS.s3credsField HiddenField
		]
	, setup = glacierSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

datacenterField :: RemoteConfigField
datacenterField = Accepted "datacenter"
	
vaultField :: RemoteConfigField
vaultField = Accepted "vault"

fileprefixField :: RemoteConfigField
fileprefixField = Accepted "fileprefix"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = new 
	<$> parsedRemoteConfig remote rc
	<*> remoteCost gc veryExpensiveRemoteCost
  where
	new c cst = Just $ specialRemote' specialcfg c
		(store this)
		(retrieve this)
		(remove this)
		(checkKey this)
		this
	  where
		this = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileCheap = Nothing
			-- glacier-cli does not follow redirects and does
			-- not support file://, as far as we know, but
			-- there's no guarantee that will continue to be
			-- the case, so require verifiable keys.
			, retrievalSecurityPolicy = mkRetrievalVerifiableKeysSecure gc
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = exportUnsupported
			, importActions = importUnsupported
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Nothing
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = return Nothing
			, getInfo = includeCredsInfo c (AWS.creds u) $
				[ ("glacier vault", getVault c) ]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
		specialcfg = (specialRemoteCfg c)
			-- Disabled until jobList gets support for chunks.
			{ chunkConfig = NoChunks
			}

glacierSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
glacierSetup ss mu mcreds c gc = do
	u <- maybe (liftIO genUUID) return mu
	glacierSetup' ss u mcreds c gc
glacierSetup' :: SetupStage -> UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
glacierSetup' ss u mcreds c gc = do
	(c', encsetup) <- encryptionSetup (c `M.union` defaults) gc
	pc <- either giveup return . parseRemoteConfig c'
		=<< configParser remote c'
	c'' <- setRemoteCredPair ss encsetup pc gc (AWS.creds u) mcreds
	pc' <- either giveup return . parseRemoteConfig c''
		=<< configParser remote c''
	case ss of
		Init -> genVault pc' gc u
		_ -> return ()
	gitConfigSpecialRemote u c'' [("glacier", "true")]
	return (c'', u)
  where
	remotename = fromJust (lookupName c)
	defvault = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ (datacenterField, Proposed $ T.unpack $ AWS.defaultRegion AWS.Glacier)
		, (vaultField, Proposed defvault)
		]

store :: Remote -> Storer
store r k b p = do
	checkNonEmpty k
	byteStorer (store' r) k b p

checkNonEmpty :: Key -> Annex ()
checkNonEmpty k
	| fromKey keySize k == Just 0 =
		giveup "Cannot store empty files in Glacier."
	| otherwise = return ()

store' :: Remote -> Key -> L.ByteString -> MeterUpdate -> Annex ()
store' r k b p = go =<< glacierEnv c gc u
  where
	c = config r
	gc = gitconfig r
	u = uuid r
	params = glacierParams c
		[ Param "archive"
		, Param "upload"
		, Param "--name", Param $ archive r k
		, Param $ getVault $ config r
		, Param "-"
		]
	go Nothing = giveup "Glacier not usable."
	go (Just e) =
		let cmd = (proc "glacier" (toCommand params)) { env = Just e }
			{ std_in = CreatePipe }
		in liftIO $ withCreateProcess cmd (go' cmd)
	go' cmd (Just hin) _ _ pid = do
		meteredWrite p (S.hPut hin) b
		hClose hin
		forceSuccessProcess cmd pid
	go' _ _ _ _ _ = error "internal"

retrieve :: Remote -> Retriever
retrieve = byteRetriever . retrieve'

retrieve' :: Remote -> Key -> (L.ByteString -> Annex ()) -> Annex ()
retrieve' r k sink = go =<< glacierEnv c gc u
  where
	c = config r
	gc = gitconfig r
	u = uuid r
	params = glacierParams c
		[ Param "archive"
		, Param "retrieve"
		, Param "-o-"
		, Param $ getVault $ config r
		, Param $ archive r k
		]
	go Nothing = giveup "cannot retrieve from glacier"
	go (Just environ) = do
		let p = (proc "glacier" (toCommand params))
			{ env = Just environ
			, std_out = CreatePipe
			}
		bracketIO (createProcess p) cleanupProcess (go' p)
	go' p (_, Just h, _, pid) = do
		let cleanup = liftIO $ do
			hClose h
			forceSuccessProcess p pid
		flip finally cleanup $ do
			-- Glacier cannot store empty files, so if
			-- the output is empty, the content is not
			-- available yet.
			whenM (liftIO $ hIsEOF h) $
				giveup "Content is not available from glacier yet. Recommend you wait up to 4 hours, and then run this command again."
			sink =<< liftIO (L.hGetContents h)
	go' _ _ = error "internal"

remove :: Remote -> Remover
remove r k = unlessM go $
	giveup "removal from glacier failed"
  where
	go = glacierAction r
		[ Param "archive"
		, Param "delete"
		, Param $ getVault $ config r
		, Param $ archive r k
		]

checkKey :: Remote -> CheckPresent
checkKey r k = do
	showChecking r
	go =<< glacierEnv (config r) (gitconfig r) (uuid r)
  where
	go Nothing = giveup "cannot check glacier"
	go (Just e) = do
		{- glacier checkpresent outputs the archive name to stdout if
		 - it's present. -}
		s <- liftIO $ readProcessEnv "glacier" (toCommand params) (Just e)
		let probablypresent = serializeKey k `elem` lines s
		if probablypresent
			then giveup untrusted
			else return False

	params = glacierParams (config r)
		[ Param "archive"
		, Param "checkpresent"
		, Param $ getVault $ config r
		, Param "--quiet"
		, Param $ archive r k
		]

	untrusted = unlines
			[ "Glacier's inventory says it has a copy."
			, "However, the inventory could be out of date, if it was recently removed."
			]

glacierAction :: Remote -> [CommandParam] -> Annex Bool
glacierAction r = runGlacier (config r) (gitconfig r) (uuid r)

runGlacier :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> [CommandParam] -> Annex Bool
runGlacier c gc u params = go =<< glacierEnv c gc u
  where
	go Nothing = return False
	go (Just e) = liftIO $
		boolSystemEnv "glacier" (glacierParams c params) (Just e)

glacierParams :: ParsedRemoteConfig -> [CommandParam] -> [CommandParam]
glacierParams c params = datacenter:params
  where
	datacenter = Param $ "--region=" ++
		fromMaybe (giveup "Missing datacenter configuration")
			(getRemoteConfigValue datacenterField c)

glacierEnv :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex (Maybe [(String, String)])
glacierEnv c gc u = do
	liftIO checkSaneGlacierCommand
	go =<< getRemoteCredPairFor "glacier" c gc creds
  where
	go Nothing = return Nothing
	go (Just (user, pass)) = do
		e <- liftIO getEnvironment
		return $ Just $ addEntries [(uk, user), (pk, pass)] e

	creds = AWS.creds u
	(uk, pk) = credPairEnvironment creds

getVault :: ParsedRemoteConfig -> Vault
getVault = fromMaybe (giveup "Missing vault configuration") 
	. getRemoteConfigValue vaultField

archive :: Remote -> Key -> Archive
archive r k = fileprefix ++ serializeKey k
  where
	fileprefix = fromMaybe "" $
		getRemoteConfigValue fileprefixField $ config r

genVault :: ParsedRemoteConfig -> RemoteGitConfig -> UUID -> Annex ()
genVault c gc u = unlessM (runGlacier c gc u params) $
	giveup "Failed creating glacier vault."
  where
	params = 
		[ Param "vault"
		, Param "create"
		, Param $ getVault c
		]

{- Partitions the input list of keys into ones which have
 - glacier retieval jobs that have succeeded, or failed.
 -
 - A complication is that `glacier job list` will display the encrypted
 - keys when the remote is encrypted.
 -
 - Dealing with encrypted chunked keys would be tricky. However, there
 - seems to be no benefit to using chunking with glacier, so chunking is
 - not supported.
 -}
jobList :: Remote -> [Key] -> Annex ([Key], [Key])
jobList r keys = go =<< glacierEnv (config r) (gitconfig r) (uuid r)
  where
	params = [ Param "job", Param "list" ]
	nada = ([], [])
	myvault = getVault $ config r

	go Nothing = return nada
	go (Just e) = do
		v <- liftIO $ catchMaybeIO $ 
			readProcessEnv "glacier" (toCommand params) (Just e)
		maybe (return nada) extract v

	extract s = do
		let result@(succeeded, failed) =
			parse nada $ (map words . lines) s
		if result == nada
			then return nada
			else do
				enckeys <- forM keys $ \k ->
					maybe k (\(_, enck) -> enck k)
						<$> cipherKey (config r) (gitconfig r)
				let keymap = M.fromList $ zip enckeys keys
				let convert = mapMaybe (`M.lookup` keymap)
				return (convert succeeded, convert failed)

	parse c [] = c
	parse c@(succeeded, failed) ((status:_date:vault:key:[]):rest)
		| vault == myvault =
			case deserializeKey key of
				Nothing -> parse c rest
				Just k
					| "a/d" `isPrefixOf` status ->
						parse (k:succeeded, failed) rest
					| "a/e" `isPrefixOf` status ->
						parse (succeeded, k:failed) rest
					| otherwise ->
						parse c rest
	parse c (_:rest) = parse c rest

-- boto's version of glacier exits 0 when given a parameter it doesn't
-- understand. See https://github.com/boto/boto/issues/2942
checkSaneGlacierCommand :: IO ()
checkSaneGlacierCommand = 
	whenM ((Nothing /=) <$> catchMaybeIO shouldfail) $
		giveup wrongcmd
  where
	test = proc "glacier" ["--compatibility-test-git-annex"]
	shouldfail = withNullHandle $ \nullh ->
		let p = test
			{ std_out = UseHandle nullh
			, std_err = UseHandle nullh
			}
		in withCreateProcess p $ \_ _ _ -> forceSuccessProcess p
	wrongcmd = "The glacier program in PATH seems to be from boto, not glacier-cli. Cannot use this program."
