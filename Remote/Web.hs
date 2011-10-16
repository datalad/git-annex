{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Web (remote) where

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Logs.Web
import qualified Utility.Url as Url
import Utility.Monad

remote :: RemoteType Annex
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
list = return [Git.repoRemoteNameSet Git.repoFromUnknown "web"]

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r _ _ = 
	return Remote {
		uuid = webUUID,
		cost = expensiveRemoteCost,
		name = Git.repoDescribe r,
		storeKey = uploadKey,
		retrieveKeyFile = downloadKey,
		removeKey = dropKey,
		hasKey = checkKey,
		hasKeyCheap = False,
		config = Nothing,
		repo = r
	}

downloadKey :: Key -> FilePath -> Annex Bool
downloadKey key file = get =<< getUrls key
	where
		get [] = do
			warning "no known url"
			return False
		get urls = do
			showOutput -- make way for download progress bar
			liftIO $ anyM (`Url.download` file) urls

uploadKey :: Key -> Annex Bool
uploadKey _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey _ = do
	warning "removal from web not supported"
	return False

checkKey :: Key -> Annex (Either IOException Bool)
checkKey key = do
	us <- getUrls key
	if null us
		then return $ Right False
		else return . Right =<< checkKey' us
checkKey' :: [URLString] -> Annex Bool
checkKey' [] = return False
checkKey' (u:us) = do
	showAction $ "checking " ++ u
	e <- liftIO $ Url.exists u
	if e then return e else checkKey' us
