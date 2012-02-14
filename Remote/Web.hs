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
import qualified Git.Construct
import Annex.Content
import Config
import Logs.Web
import qualified Utility.Url as Url
import Types.Key

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

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r _ _ = 
	return Remote {
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
		config = Nothing,
		repo = r,
		remotetype = remote
	}

downloadKey :: Key -> FilePath -> Annex Bool
downloadKey key file = get =<< getUrls key
	where
		get [] = do
			warning "no known url"
			return False
		get urls = do
			showOutput -- make way for download progress bar
			downloadUrl urls file

downloadKeyCheap :: Key -> FilePath -> Annex Bool
downloadKeyCheap _ _ = return False

uploadKey :: Key -> Annex Bool
uploadKey _ = do
	warning "upload to web not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey _ = do
	warning "removal from web not supported"
	return False

checkKey :: Key -> Annex (Either String Bool)
checkKey key = do
	us <- getUrls key
	if null us
		then return $ Right False
		else return . Right =<< checkKey' key us
checkKey' :: Key -> [URLString] -> Annex Bool
checkKey' key us = untilTrue us $ \u -> do
	showAction $ "checking " ++ u
	liftIO $ Url.check u (keySize key)
