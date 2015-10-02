{- Web remote.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Web (remote, getWebUrls) where

import Common.Annex
import Types.Remote
import Remote.Helper.Messages
import qualified Git
import qualified Git.Construct
import Annex.Content
import Config.Cost
import Logs.Web
import Annex.UUID
import Types.Key
import Utility.Metered
import qualified Annex.Url as Url
#ifdef WITH_QUVI
import Annex.Quvi
import qualified Utility.Quvi as Quvi
#endif

remote :: RemoteType
remote = RemoteType {
	typename = "web",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

-- There is only one web remote, and it always exists.
-- (If the web should cease to exist, remove this module and redistribute
-- a new release to the survivors by carrier pigeon.)
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "web" (pure Git.Construct.fromUnknown)
	return [r]

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r _ c gc = 
	return $ Just Remote
		{ uuid = webUUID
		, cost = expensiveRemoteCost
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey
		, retrieveKeyFileCheap = downloadKeyCheap
		, removeKey = dropKey
		, checkPresent = checkKey
		, checkPresentCheap = False
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, repo = r
		, readonly = True
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Nothing -- implicitly claims all urls
		, checkUrl = Nothing
		}

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
downloadKey key _file dest _p = unVerified $ get =<< getWebUrls key
  where
	get [] = do
		warning "no known url"
		return False
	get urls = do
		showOutput -- make way for download progress bar
		untilTrue urls $ \u -> do
			let (u', downloader) = getDownloader u
			case downloader of
				QuviDownloader -> do
#ifdef WITH_QUVI
					flip downloadUrl dest
						=<< withQuviOptions Quvi.queryLinks [Quvi.httponly, Quvi.quiet] u'
#else
					warning "quvi support needed for this url"
					return False
#endif
				_ -> downloadUrl [u'] dest

downloadKeyCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
downloadKeyCheap _ _ _ = return False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
uploadKey _ _ _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey k = do
	mapM_ (setUrlMissing webUUID k) =<< getWebUrls k
	return True

checkKey :: Key -> Annex Bool
checkKey key = do
	us <- getWebUrls key
	if null us
		then return False
		else either error return =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex (Either String Bool)
checkKey' key us = firsthit us (Right False) $ \u -> do
	let (u', downloader) = getDownloader u
	showChecking u'
	case downloader of
		QuviDownloader ->
#ifdef WITH_QUVI
			Right <$> withQuviOptions Quvi.check [Quvi.httponly, Quvi.quiet] u'
#else
			return $ Left "quvi support needed for this url"
#endif
		_ -> do
			Url.withUrlOptions $ catchMsgIO .
				Url.checkBoth u' (keySize key)
  where
	firsthit [] miss _ = return miss
	firsthit (u:rest) _ a = do
		r <- a u
		case r of
			Right _ -> return r
			Left _ -> firsthit rest r a

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = filter supported <$> getUrls key
  where
	supported u = snd (getDownloader u) 
		`elem` [WebDownloader, QuviDownloader]
