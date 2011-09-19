{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Web (
	remote,
	setUrl
) where

import Control.Monad.State (liftIO)
import Control.Exception
import System.FilePath

import Types
import Types.Remote
import qualified Git
import qualified Annex
import Messages
import UUID
import Config
import PresenceLog
import LocationLog
import Locations
import Utility
import qualified Utility.Url as Url

type URLString = String

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
list = return [Git.repoRemoteNameSet Git.repoFromUnknown "remote.web.dummy"]

-- Dummy uuid for the whole web. Do not alter.
webUUID :: UUID
webUUID = "00000000-0000-0000-0000-000000000001"

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

{- The urls for a key are stored in remote/web/hash/key.log 
 - in the git-annex branch. -}
urlLog :: Key -> FilePath
urlLog key = "remote/web" </> hashDirLower key </> keyFile key ++ ".log"
oldurlLog :: Key -> FilePath
{- A bug used to store the urls elsewhere. -}
oldurlLog key = "remote/web" </> hashDirLower key </> show key ++ ".log"

getUrls :: Key -> Annex [URLString]
getUrls key = do
	us <- currentLog (urlLog key)
	if null us
		then currentLog (oldurlLog key)
		else return us

{- Records a change in an url for a key. -}
setUrl :: Key -> URLString -> LogStatus -> Annex ()
setUrl key url status = do
	g <- Annex.gitRepo
	addLog (urlLog key) =<< logNow status url

	-- update location log to indicate that the web has the key, or not
	us <- getUrls key
	logChange g key webUUID (if null us then InfoMissing else InfoPresent)

downloadKey :: Key -> FilePath -> Annex Bool
downloadKey key file = get =<< getUrls key
	where
		get [] = do
			warning "no known url"
			return False
		get urls = anyM (`Url.download` file) urls

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
