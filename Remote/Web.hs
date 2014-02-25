{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Web (remote) where

import Common.Annex
import Types.Remote
import qualified Git
import qualified Git.Construct
import Annex.Content
import Config
import Config.Cost
import Logs.Web
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
list :: Annex [Git.Repo]
list = do
	r <- liftIO $ Git.Construct.remoteNamed "web" Git.Construct.fromUnknown
	return [r]

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r _ c gc = 
	return $ Just Remote {
		uuid = webUUID,
		cost = expensiveRemoteCost,
		name = Git.repoDescribe r,
		storeKey = uploadKey,
		retrieveKeyFile = downloadKey,
		retrieveKeyFileCheap = downloadKeyCheap,
		removeKey = dropKey,
		hasKey = checkKey,
		hasKeyCheap = False,
		whereisKey = Just getUrls,
		remoteFsck = Nothing,
		repairRepo = Nothing,
		config = c,
		gitconfig = gc,
		localpath = Nothing,
		repo = r,
		readonly = True,
		availability = GloballyAvailable,
		remotetype = remote
	}

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
downloadKey key _file dest _p = get =<< getUrls key
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
				DefaultDownloader -> downloadUrl [u'] dest

downloadKeyCheap :: Key -> FilePath -> Annex Bool
downloadKeyCheap _ _ = return False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
uploadKey _ _ _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey k = do
	mapM_ (setUrlMissing k) =<< getUrls k
	return True

checkKey :: Key -> Annex (Either String Bool)
checkKey key = do
	us <- getUrls key
	if null us
		then return $ Right False
		else return =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex (Either String Bool)
checkKey' key us = firsthit us (Right False) $ \u -> do
	let (u', downloader) = getDownloader u
	showAction $ "checking " ++ u'
	case downloader of
		QuviDownloader ->
#ifdef WITH_QUVI
			Right <$> withQuviOptions Quvi.check [Quvi.httponly, Quvi.quiet] u'
#else
			return $ Left "quvi support needed for this url"
#endif
		DefaultDownloader -> do
			(headers, options) <- getHttpHeadersOptions
			Url.withUserAgent $ catchMsgIO .
				Url.checkBoth u' headers options (keySize key)
  where
  	firsthit [] miss _ = return miss
	firsthit (u:rest) _ a = do
		r <- a u
		case r of
			Right _ -> return r
			Left _ -> firsthit rest r a
