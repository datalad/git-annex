{- Amazon Glacier remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Glacier (remote, jobList) where

import qualified Data.Map as M
import qualified Data.Text as T

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Encryptable
import qualified Remote.Helper.AWS as AWS
import Crypto
import Creds
import Utility.Metered
import qualified Annex
import Annex.Content
import Annex.UUID
import Utility.Env

import System.Process

type Vault = String
type Archive = FilePath

remote :: RemoteType
remote = RemoteType {
	typename = "glacier",
	enumerate = findSpecialRemotes "glacier",
	generate = gen,
	setup = glacierSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = new <$> remoteCost gc veryExpensiveRemoteCost
  where
	new cst = Just $ encryptableRemote c
		(storeEncrypted this)
		(retrieveEncrypted this)
		this
	  where
		this = Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store this,
			retrieveKeyFile = retrieve this,
			retrieveKeyFileCheap = retrieveCheap this,
			removeKey = remove this,
			hasKey = checkPresent this,
			hasKeyCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Nothing,
			readonly = False,
			availability = GloballyAvailable,
			remotetype = remote
		}

glacierSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
glacierSetup mu c = do
	u <- maybe (liftIO genUUID) return mu
	glacierSetup' u c
glacierSetup' :: UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
glacierSetup' u c = do
	c' <- encryptionSetup c
	let fullconfig = c' `M.union` defaults
	genVault fullconfig u
	gitConfigSpecialRemote u fullconfig "glacier" "true"
	c'' <- setRemoteCredPair fullconfig (AWS.creds u)
	return (c'', u)
  where
	remotename = fromJust (M.lookup "name" c)
	defvault = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ ("datacenter", T.unpack $ AWS.defaultRegion AWS.Glacier)
		, ("vault", defvault)
		]

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f p
	| keySize k == Just 0 = do
		warning "Cannot store empty files in Glacier."
		return False
	| otherwise = sendAnnex k (void $ remove r k) $ \src ->
		metered (Just p) k $ \meterupdate ->
			storeHelper r k $ streamMeteredFile src meterupdate

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k p = sendAnnex k (void $ remove r enck) $ \src ->
	metered (Just p) k $ \meterupdate ->
		storeHelper r enck $ \h ->
			encrypt (getGpgEncParams r) cipher (feedFile src)
				(readBytes $ meteredWrite meterupdate h)

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve r k _f d p = metered (Just p) k $ \meterupdate ->
	retrieveHelper r k $
		readBytes $ meteredWriteFile meterupdate d

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted r (cipher, enck) k d p = metered (Just p) k $ \meterupdate ->
	retrieveHelper r enck $ readBytes $ \b ->
		decrypt cipher (feedBytes b) $
			readBytes $ meteredWriteFile meterupdate d

storeHelper :: Remote -> Key -> (Handle -> IO ()) -> Annex Bool
storeHelper r k feeder = go =<< glacierEnv c u
  where
	c = config r
	u = uuid r
	params = glacierParams c
		[ Param "archive"
		, Param "upload"
		, Param "--name", Param $ archive r k
		, Param $ getVault $ config r
		, Param "-"
		]
	go Nothing = return False
	go (Just e) = do
		let p = (proc "glacier" (toCommand params)) { env = Just e }
		liftIO $ catchBoolIO $
			withHandle StdinHandle createProcessSuccess p $ \h -> do
				feeder h
				return True

retrieveHelper :: Remote -> Key -> (Handle -> IO ()) -> Annex Bool
retrieveHelper r k reader = go =<< glacierEnv c u
  where
	c = config r
	u = uuid r
	params = glacierParams c
		[ Param "archive"
		, Param "retrieve"
		, Param "-o-"
		, Param $ getVault $ config r
		, Param $ archive r k
		]
	go Nothing = return False
	go (Just e) = do
		let p = (proc "glacier" (toCommand params)) { env = Just e }
		ok <- liftIO $ catchBoolIO $
			withHandle StdoutHandle createProcessSuccess p $ \h ->
				ifM (hIsEOF h)
					( return False
					, do
						reader h
						return True
					)
		unless ok later
		return ok
	later = showLongNote "Recommend you wait up to 4 hours, and then run this command again."

remove :: Remote -> Key -> Annex Bool
remove r k = glacierAction r
	[ Param "archive"
	, Param "delete"
	, Param $ getVault $ config r
	, Param $ archive r k
	]

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = do
	showAction $ "checking " ++ name r
	go =<< glacierEnv (config r) (uuid r)
  where
	go Nothing = return $ Left "cannot check glacier"
	go (Just e) = do
		{- glacier checkpresent outputs the archive name to stdout if
		 - it's present. -}
		v <- liftIO $ catchMsgIO $ 
			readProcessEnv "glacier" (toCommand params) (Just e)
		case v of
			Right s -> do
				let probablypresent = key2file k `elem` lines s
				if probablypresent
					then ifM (Annex.getFlag "trustglacier")
						( return $ Right True, untrusted )
					else return $ Right False
			Left err -> return $ Left err

	params =
		[ Param "archive"
		, Param "checkpresent"
		, Param $ getVault $ config r
		, Param "--quiet"
		, Param $ archive r k
		]

	untrusted = return $ Left $ unlines
			[ "Glacier's inventory says it has a copy."
			, "However, the inventory could be out of date, if it was recently removed."
			, "(Use --trust-glacier if you're sure it's still in Glacier.)"
			, ""
			]

glacierAction :: Remote -> [CommandParam] -> Annex Bool
glacierAction r = runGlacier (config r) (uuid r)

runGlacier :: RemoteConfig -> UUID -> [CommandParam] -> Annex Bool
runGlacier c u params = go =<< glacierEnv c u
  where
	go Nothing = return False
	go (Just e) = liftIO $
		boolSystemEnv "glacier" (glacierParams c params) (Just e)

glacierParams :: RemoteConfig -> [CommandParam] -> [CommandParam]
glacierParams c params = datacenter:params
  where
	datacenter = Param $ "--region=" ++
		fromJust (M.lookup "datacenter" c)

glacierEnv :: RemoteConfig -> UUID -> Annex (Maybe [(String, String)])
glacierEnv c u = go =<< getRemoteCredPairFor "glacier" c creds
  where
	go Nothing = return Nothing
	go (Just (user, pass)) = do
		e <- liftIO getEnvironment
		return $ Just $ addEntries [(uk, user), (pk, pass)] e

	creds = AWS.creds u
	(uk, pk) = credPairEnvironment creds

getVault :: RemoteConfig -> Vault
getVault = fromJust . M.lookup "vault"

archive :: Remote -> Key -> Archive
archive r k = fileprefix ++ key2file k
  where
	fileprefix = M.findWithDefault "" "fileprefix" $ config r

-- glacier vault create will succeed even if the vault already exists.
genVault :: RemoteConfig -> UUID -> Annex ()
genVault c u = unlessM (runGlacier c u params) $
	error "Failed creating glacier vault."
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
 -}
jobList :: Remote -> [Key] -> Annex ([Key], [Key])
jobList r keys = go =<< glacierEnv (config r) (uuid r)
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
					maybe k snd <$> cipherKey (config r) k
				let keymap = M.fromList $ zip enckeys keys
				let convert = mapMaybe (`M.lookup` keymap)
				return (convert succeeded, convert failed)

	parse c [] = c
	parse c@(succeeded, failed) ((status:_date:vault:key:[]):rest)
		| vault == myvault =
			case file2key key of
				Nothing -> parse c rest
				Just k
					| "a/d" `isPrefixOf` status ->
						parse (k:succeeded, failed) rest
					| "a/e" `isPrefixOf` status ->
						parse (succeeded, k:failed) rest
					| otherwise ->
						parse c rest
	parse c (_:rest) = parse c rest
