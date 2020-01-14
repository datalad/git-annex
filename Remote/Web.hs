{- Web remote.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Web (remote, getWebUrls) where

import Annex.Common
import Types.Remote
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import qualified Git
import qualified Git.Construct
import Annex.Content
import Config.Cost
import Config
import Logs.Web
import Annex.UUID
import Utility.Metered
import qualified Annex.Url as Url
import Annex.YoutubeDl

remote :: RemoteType
remote = RemoteType
	{ typename = "web"
	, enumerate = list
	, generate = gen
	, configParser = mkRemoteConfigParser []
	, setup = error "not supported"
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

-- There is only one web remote, and it always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "web" (pure Git.Construct.fromUnknown)
	return [r]

gen :: Git.Repo -> UUID -> ParsedRemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r _ c gc rs = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just Remote
		{ uuid = webUUID
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey
		, retrieveKeyFileCheap = downloadKeyCheap
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
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing -- implicitly claims all urls
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
downloadKey key _af dest p = unVerified $ get =<< getWebUrls key
  where
	get [] = do
		warning "no known url"
		return False
	get urls = untilTrue urls $ \u -> do
		let (u', downloader) = getDownloader u
		case downloader of
			YoutubeDownloader -> do
				showOutput
				youtubeDlTo key u' dest
			_ -> downloadUrl key p [u'] dest

downloadKeyCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
downloadKeyCheap _ _ _ = return False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
uploadKey _ _ _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey k = do
	mapM_ (setUrlMissing k) =<< getWebUrls k
	return True

checkKey :: Key -> Annex Bool
checkKey key = do
	us <- getWebUrls key
	if null us
		then return False
		else either giveup return =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex (Either String Bool)
checkKey' key us = firsthit us (Right False) $ \u -> do
	let (u', downloader) = getDownloader u
	showChecking u'
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
