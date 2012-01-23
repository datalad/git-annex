{- Amazon S3 remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.S3 (remote) where

import Network.AWS.AWSConnection
import Network.AWS.S3Object
import Network.AWS.S3Bucket hiding (size)
import Network.AWS.AWSResult
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Data.Char
import System.Environment
import System.Posix.Env (setEnv)

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Annex.Content
import Utility.Base64

remote :: RemoteType
remote = RemoteType {
	typename = "S3",
	enumerate = findSpecialRemotes "s3",
	generate = gen,
	setup = s3Setup
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
			removeKey = remove this,
			hasKey = checkPresent this,
			hasKeyCheap = False,
			config = c,
			repo = r,
			remotetype = remote
		}

s3Setup :: UUID -> RemoteConfig -> Annex RemoteConfig
s3Setup u c = handlehost $ M.lookup "host" c
	where
		remotename = fromJust (M.lookup "name" c)
		defbucket = remotename ++ "-" ++ fromUUID u
		defaults = M.fromList
			[ ("datacenter", "US")
			, ("storageclass", "STANDARD")
			, ("host", defaultAmazonS3Host)
			, ("port", show defaultAmazonS3Port)
			, ("bucket", defbucket)
			]
		
		handlehost Nothing = defaulthost
		handlehost (Just h)
			| ".archive.org" `isSuffixOf` map toLower h = archiveorg
			| otherwise = defaulthost

		use fullconfig = do
			gitConfigSpecialRemote u fullconfig "s3" "true"
			s3SetCreds fullconfig	

		defaulthost = do
			c' <- encryptionSetup c
			let fullconfig = c' `M.union` defaults
			genBucket fullconfig
			use fullconfig

		archiveorg = do
			showNote "Internet Archive mode"
			maybe (error "specify bucket=") (const $ return ()) $
				M.lookup "bucket" archiveconfig
			use archiveconfig
			where
				archiveconfig =
					-- hS3 does not pass through
					-- x-archive-* headers
					M.mapKeys (replace "x-archive-" "x-amz-") $
					-- encryption does not make sense here
					M.insert "encryption" "none" $
					M.union c $
					-- special constraints on key names
					M.insert "mungekeys" "ia" $
					-- bucket created only when files
					-- are uploaded
					M.insert "x-amz-auto-make-bucket" "1" $
					-- no default bucket name; should
					-- be human-readable
					M.delete "bucket" defaults

store :: Remote -> Key -> Annex Bool
store r k = s3Action r False $ \(conn, bucket) -> do
	dest <- inRepo $ gitAnnexLocation k
	res <- liftIO $ storeHelper (conn, bucket) r k dest
	s3Bool res

storeEncrypted :: Remote -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted r (cipher, enck) k = s3Action r False $ \(conn, bucket) -> 
	-- To get file size of the encrypted content, have to use a temp file.
	-- (An alternative would be chunking to to a constant size.)
	withTmp enck $ \tmp -> do
		f <- inRepo $ gitAnnexLocation k
		liftIO $ withEncryptedContent cipher (L.readFile f) $ \s -> L.writeFile tmp s
		res <- liftIO $ storeHelper (conn, bucket) r enck tmp
		s3Bool res

storeHelper :: (AWSConnection, String) -> Remote -> Key -> FilePath -> IO (AWSResult ())
storeHelper (conn, bucket) r k file = do
	content <- liftIO $ L.readFile file
	-- size is provided to S3 so the whole content does not need to be
	-- buffered to calculate it
	size <- maybe getsize (return . fromIntegral) $ keySize k
	let object = setStorageClass storageclass $ 
		S3Object bucket (bucketFile r k) ""
			(("Content-Length", show size) : xheaders) content
	sendObject conn object
	where
		storageclass =
			case fromJust $ M.lookup "storageclass" $ fromJust $ config r of
				"REDUCED_REDUNDANCY" -> REDUCED_REDUNDANCY
				_ -> STANDARD
		getsize = do
			s <- liftIO $ getFileStatus file
			return $ fileSize s
		
		xheaders = filter isxheader $ M.assocs $ fromJust $ config r
		isxheader (h, _) = "x-amz-" `isPrefixOf` h

retrieve :: Remote -> Key -> FilePath -> Annex Bool
retrieve r k f = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey r bucket k
	case res of
		Right o -> do
			liftIO $ L.writeFile f $ obj_data o
			return True
		Left e -> s3Warning e

retrieveEncrypted :: Remote -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted r (cipher, enck) f = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ getObject conn $ bucketKey r bucket enck
	case res of
		Right o -> liftIO $ 
			withDecryptedContent cipher (return $ obj_data o) $ \content -> do
				L.writeFile f content
				return True
		Left e -> s3Warning e

remove :: Remote -> Key -> Annex Bool
remove r k = s3Action r False $ \(conn, bucket) -> do
	res <- liftIO $ deleteObject conn $ bucketKey r bucket k
	s3Bool res

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k = s3Action r noconn $ \(conn, bucket) -> do
	showAction $ "checking " ++ name r
	res <- liftIO $ getObjectInfo conn $ bucketKey r bucket k
	case res of
		Right _ -> return $ Right True
		Left (AWSError _ _) -> return $ Right False
		Left e -> return $ Left (s3Error e)
	where
		noconn = Left $ error "S3 not configured"
			
s3Warning :: ReqError -> Annex Bool
s3Warning e = do
	warning $ prettyReqError e
	return False

s3Error :: ReqError -> a
s3Error e = error $ prettyReqError e

s3Bool :: AWSResult () -> Annex Bool
s3Bool (Right _) = return True
s3Bool (Left e) = s3Warning e

s3Action :: Remote -> a -> ((AWSConnection, String) -> Annex a) -> Annex a
s3Action r noconn action = do
	when (isNothing $ config r) $
		error $ "Missing configuration for special remote " ++ name r
	let bucket = M.lookup "bucket" $ fromJust $ config r
	conn <- s3Connection $ fromJust $ config r
	case (bucket, conn) of
		(Just b, Just c) -> action (c, b)
		_ -> return noconn

bucketFile :: Remote -> Key -> FilePath
bucketFile r = munge . show
	where
		munge s = case M.lookup "mungekeys" $ fromJust $ config r of
			Just "ia" -> iaMunge s
			_ -> s

bucketKey :: Remote -> String -> Key -> S3Object
bucketKey r bucket k = S3Object bucket (bucketFile r k) "" [] L.empty

{- Internet Archive limits filenames to a subset of ascii,
 - with no whitespace. Other characters are xml entity
 - encoded. -}
iaMunge :: String -> String
iaMunge = (>>= munge)
	where
		munge c
			| isAsciiUpper c || isAsciiLower c || isNumber c = [c]
			| c `elem` "_-.\"" = [c]
			| isSpace c = []
			| otherwise = "&" ++ show (ord c) ++ ";"

genBucket :: RemoteConfig -> Annex ()
genBucket c = do
	conn <- s3ConnectionRequired c
	showAction "checking bucket"
	loc <- liftIO $ getBucketLocation conn bucket 
	case loc of
		Right _ -> return ()
		Left err@(NetworkError _) -> s3Error err
		Left (AWSError _ _) -> do
			showAction $ "creating bucket in " ++ datacenter
			res <- liftIO $ createBucketIn conn bucket datacenter
			case res of
				Right _ -> return ()
				Left err -> s3Error err
	where
		bucket = fromJust $ M.lookup "bucket" c
		datacenter = fromJust $ M.lookup "datacenter" c

s3ConnectionRequired :: RemoteConfig -> Annex AWSConnection
s3ConnectionRequired c =
	maybe (error "Cannot connect to S3") return =<< s3Connection c

s3Connection :: RemoteConfig -> Annex (Maybe AWSConnection)
s3Connection c = do
	creds <- s3GetCreds c
	case creds of
		Just (ak, sk) -> return $ Just $ AWSConnection host port ak sk
		_ -> do
			warning $ "Set both " ++ s3AccessKey ++ " and " ++ s3SecretKey  ++ " to use S3"
			return Nothing
	where
		host = fromJust $ M.lookup "host" c
		port = let s = fromJust $ M.lookup "port" c in
			case reads s of
			[(p, _)] -> p
			_ -> error $ "bad S3 port value: " ++ s

{- S3 creds come from the environment if set. 
 - Otherwise, might be stored encrypted in the remote's config. -}
s3GetCreds :: RemoteConfig -> Annex (Maybe (String, String))
s3GetCreds c = do
	ak <- getEnvKey s3AccessKey
	sk <- getEnvKey s3SecretKey
	if null ak || null sk
		then do
			mcipher <- remoteCipher c
			case (M.lookup "s3creds" c, mcipher) of
				(Just encrypted, Just cipher) -> do
					s <- liftIO $ withDecryptedContent cipher
						(return $ L.pack $ fromB64 encrypted)
						(return . L.unpack)
					let [ak', sk', _rest] = lines s
					liftIO $ do
						setEnv s3AccessKey ak True
						setEnv s3SecretKey sk True
					return $ Just (ak', sk')
				_ -> return Nothing
		else return $ Just (ak, sk)
	where
		getEnvKey s = liftIO $ catchDefaultIO (getEnv s) ""

{- Stores S3 creds encrypted in the remote's config if possible. -}
s3SetCreds :: RemoteConfig -> Annex RemoteConfig
s3SetCreds c = do
	creds <- s3GetCreds c
	case creds of
		Just (ak, sk) -> do
			mcipher <- remoteCipher c
			case mcipher of
				Just cipher -> do
					s <- liftIO $ withEncryptedContent cipher
						(return $ L.pack $ unlines [ak, sk])
						(return . L.unpack)
					return $ M.insert "s3creds" (toB64 s) c
				Nothing -> return c
		_ -> return c

s3AccessKey :: String
s3AccessKey = "AWS_ACCESS_KEY_ID"
s3SecretKey :: String
s3SecretKey = "AWS_SECRET_ACCESS_KEY"
