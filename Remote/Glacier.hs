{- Amazon Glacier remotes.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Glacier (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import System.Environment

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import qualified Remote.Helper.AWS as AWS
import Crypto
import Creds
import Annex.Content
import qualified Annex

type Vault = String
type Archive = FilePath

remote :: RemoteType
remote = RemoteType {
	typename = "glacier",
	enumerate = findSpecialRemotes "glacier",
	generate = gen,
	setup = glacierSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	cst <- remoteCost r expensiveRemoteCost
	return $ gen' r u c cst
gen' :: Git.Repo -> UUID -> Maybe RemoteConfig -> Int -> Remote
gen' r u c cst =
	encryptableRemote c
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
		config = c,
		repo = r,
		localpath = Nothing,
		readonly = False,
		remotetype = remote
	}

glacierSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
glacierSetup u c = do
	c' <- encryptionSetup c
	let fullconfig = c' `M.union` defaults
	genVault fullconfig u
	gitConfigSpecialRemote u fullconfig "glacier" "true"
	setRemoteCredPair fullconfig (AWS.creds u)
  where
	remotename = fromJust (M.lookup "name" c)
	defvault = remotename ++ "-" ++ fromUUID u
	defaults = M.fromList
		[ ("datacenter", "us-east-1")
		, ("vault", defvault)
		]

store :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r k _f _p
	| keySize k == Just 0 = do
		warning "Cannot store empty files in Glacier."
		return False
	| otherwise = do
	src <- inRepo $ gitAnnexLocation k
	storeHelper r k src

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r (cipher, enck) k _p = 
	-- With current glacier-cli UI, have to encrypt to a temp file.
	withTmp enck $ \tmp -> do
		f <- inRepo $ gitAnnexLocation k
		liftIO $ encrypt cipher (feedFile f) $
			readBytes $ L.writeFile tmp
		storeHelper r enck tmp

{- Glacier cannot store empty files. So empty keys are handled by
 - doing nothing on storage, and re-creating the empty file on retrieve. -}
storeHelper :: Remote -> Key -> FilePath -> Annex Bool
storeHelper r k file = do
	showOutput
	glacierAction r
		[ Param "archive"
		, Param "upload"
		, Param "--name", Param $ archive r k
		, Param $ remoteVault r
		, File file
		]

retrieve :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve r k _f d = retrieveHelper r k d

retrieveCheap :: Remote -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: Remote -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) _k d = do
	withTmp enck $ \tmp -> do
		ok <- retrieveHelper r enck tmp
		if ok
			then liftIO $ decrypt cipher (feedFile tmp) $
				readBytes $ \content -> do
					L.writeFile d content
					return True
			else return False

retrieveHelper :: Remote -> Key -> FilePath -> Annex Bool
retrieveHelper r k file = do
	showOutput
	ok <- glacierAction r
		[ Param "archive"
		, Param "retrieve"
		, Param "-o", File file
		, Param $ remoteVault r
		, Param $ archive r k
		]
	unless ok $
		showLongNote "Recommend you wait up to 4 hours, and then run this command again."
	return ok

remove :: Remote -> Key -> Annex Bool
remove r k = glacierAction r
	[ Param "archive"
	, Param "delete"
	, Param $ remoteVault r
	, Param $ archive r k
	]

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = do
	showAction $ "checking " ++ name r
	go =<< glacierEnv (fromJust $ config r) (uuid r)
  where
	go Nothing = return $ Left "cannot check glacier"
	go (Just env) = do
		{- glacier checkpresent outputs the archive name to stdout if
		 - it's present. -}
		v <- liftIO $ catchMsgIO $ 
			readProcessEnv "glacier" (toCommand params) (Just env)
		case v of
			Right s -> do
				let probablypresent = key2file k `elem` lines s
				if probablypresent
					then ifM (Annex.getFlag "trustglacier")
						( return $ Right True, untrusted )
					else return $ Right False
			Left e -> return $ Left e

	params =
		[ Param "archive"
		, Param "checkpresent"
		, Param $ remoteVault r
		, Param $ archive r k
		]

	untrusted = do
		showLongNote $ unlines
			[ "Glacier's inventory says it has a copy."
			, "However, the inventory could be out of date, if it was recently removed."
			, "(Use --trust-glacier if you're sure it's still in Glacier.)"
			, ""
			]
		return $ Right False

glacierAction :: Remote -> [CommandParam] -> Annex Bool
glacierAction r params = do
	when (isNothing $ config r) $
		error $ "Missing configuration for special remote " ++ name r
	runGlacier (fromJust $ config r) (uuid r) params

runGlacier :: RemoteConfig -> UUID -> [CommandParam] -> Annex Bool
runGlacier c u params = go =<< glacierEnv c u
  where
	go Nothing = return False
	go (Just env) = liftIO $
		boolSystemEnv "glacier" (datacenter:params) (Just env)

	datacenter = Param $ "--region=" ++
		(fromJust $ M.lookup "datacenter" c)

glacierEnv :: RemoteConfig -> UUID -> Annex (Maybe [(String, String)])
glacierEnv c u = go =<< getRemoteCredPair "glacier" c creds
  where
	go Nothing = return Nothing
	go (Just (user, pass)) = do
		env <- liftIO getEnvironment
		return $ Just $ (uk, user):(pk, pass):env

	creds = AWS.creds u
	(uk, pk) = credPairEnvironment creds

remoteVault :: Remote -> Vault
remoteVault = vault . fromJust . config

vault :: RemoteConfig -> Vault
vault = fromJust . M.lookup "vault"

archive :: Remote -> Key -> Archive
archive r k = fileprefix ++ key2file k
  where
	fileprefix = M.findWithDefault "" "fileprefix" $ fromJust $ config r

-- glacier vault create will succeed even if the vault already exists.
genVault :: RemoteConfig -> UUID -> Annex ()
genVault c u = unlessM (runGlacier c u params) $
	error "Failed creating glacier vault."
  where
	params = 
		[ Param "vault"
		, Param "create"
		, Param $ vault c
		]
