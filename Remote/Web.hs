{- Web remote.
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Web (remote, getWebUrls) where

import Annex.Common
import Types.Remote
import Remote.Helper.Special
import Remote.Helper.ExportImport
import qualified Git
import qualified Git.Construct
import Annex.Content
import Annex.Verify
import Config.Cost
import Config
import Logs.Web
import Annex.UUID
import Utility.Metered
import qualified Annex.Url as Url
import Annex.YoutubeDl
import Annex.SpecialRemote.Config
import Types.Creds

remote :: RemoteType
remote = RemoteType
	{ typename = "web"
	, enumerate = list
	, generate = gen
	, configParser = mkRemoteConfigParser []
	, setup = setupInstance
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

-- The web remote always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
--
-- There may also be other instances of the web remote, which can be
-- limited to accessing particular urls, and have different costs.
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "web" (pure Git.Construct.fromUnknown)
	others <- findSpecialRemotes "web"
	-- List the main one last, this makes its name be used instead
	-- of the other names when git-annex is referring to content on the
	-- web.
	return (others++[r])

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just Remote
		{ uuid = if u == NoUUID then webUUID else u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey
		, retrieveKeyFileCheap = Nothing
		-- HttpManagerRestricted is used here, so this is
		-- secure.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey
		, lockContent = Nothing
		, checkPresent = checkKey
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, untrustworthy = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		-- claimingUrl makes the web special remote claim
		-- urls that are not claimed by other remotes,
		-- so no need to claim anything here.
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

setupInstance :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
setupInstance ss mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	gitConfigSpecialRemote u c [("web", "true")]
	return (c, u)

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
downloadKey key _af dest p vc = go =<< getWebUrls key
  where
	go [] = giveup "no known url"
	go urls = dl (partition (not . isyoutube) (map getDownloader urls)) >>= \case
		Just v -> return v
		Nothing -> giveup $ unwords
			[ "downloading from all"
			, show (length urls)
			, "known url(s) failed"
			]

	dl ([], ytus) = flip getM (map fst ytus) $ \u ->
		ifM (youtubeDlTo key u dest p)
			( return (Just UnVerified)
			, return Nothing
			)
	dl (us, ytus) = do
		iv <- startVerifyKeyContentIncrementally vc key
		ifM (Url.withUrlOptions $ downloadUrl True key p iv (map fst us) dest)
			( finishVerifyKeyContentIncrementally iv >>= \case
				(True, v) -> return (Just v)
				(False, _) -> dl ([], ytus)
			, dl ([], ytus)
			)

	isyoutube (_, YoutubeDownloader) = True
	isyoutube _ = False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex ()
uploadKey _ _ _ = giveup "upload to web not supported"

dropKey :: Key -> Annex ()
dropKey k = mapM_ (setUrlMissing k) =<< getWebUrls k

checkKey :: Key -> Annex Bool
checkKey key = do
	us <- getWebUrls key
	if null us
		then return False
		else either giveup return =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex (Either String Bool)
checkKey' key us = firsthit us (Right False) $ \u -> do
	let (u', downloader) = getDownloader u
	case downloader of
		YoutubeDownloader -> youtubeDlCheck u'
		_ -> catchMsgIO $
			Url.withUrlOptions $ Url.checkBoth u' (fromKey keySize key)
  where
	firsthit [] miss _ = return miss
	firsthit (u:rest) _ a = do
		r <- a u
		case r of
			Right True -> return r
			_ -> firsthit rest r a

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = filter supported <$> getUrls key
  where
	supported u = snd (getDownloader u) 
		`elem` [WebDownloader, YoutubeDownloader]
