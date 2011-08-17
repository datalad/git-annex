{- Web remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Web (
	remote,
	setUrl,
	download
) where

import Control.Monad.State (liftIO)
import Control.Exception
import System.FilePath
import Network.Browser
import Network.HTTP
import Network.URI

import Types
import Types.Remote
import qualified Git
import qualified Annex
import Messages
import Utility
import UUID
import Config
import PresenceLog
import LocationLog
import Locations

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
		config = Nothing
	}

{- The urls for a key are stored in remote/web/hash/key.log 
 - in the git-annex branch. -}
urlLog :: Key -> FilePath
urlLog key = "remote/web" </> hashDirLower key </> show key ++ ".log"

getUrls :: Key -> Annex [URLString]
getUrls key = currentLog (urlLog key)

{- Records a change in an url for a key. -}
setUrl :: Key -> URLString -> LogStatus -> Annex ()
setUrl key url status = do
	g <- Annex.gitRepo
	addLog (urlLog key) =<< logNow status url

	-- update location log to indicate that the web has the key, or not
	us <- getUrls key
	logChange g key webUUID (if null us then InfoMissing else InfoPresent)

downloadKey :: Key -> FilePath -> Annex Bool
downloadKey key file = do
	us <- getUrls key
	download us file

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
	e <- liftIO $ urlexists u
	if e then return e else checkKey' us

urlexists :: URLString -> IO Bool
urlexists url =
	case parseURI url of
		Nothing -> return False
		Just u -> do
			(_, r) <- Network.Browser.browse $ do
				setErrHandler ignore
				setOutHandler ignore
				setAllowRedirects True
				request (mkRequest HEAD u :: Request_String)
			case rspCode r of
				(2,_,_) -> return True
				_ -> return False
	where
		ignore = const $ return ()

download :: [URLString] -> FilePath -> Annex Bool
download [] _ = return False
download (url:us) file = do
	showOutput -- make way for curl progress bar
	ok <- liftIO $ boolSystem "curl" [Params "-L -C - -# -o", File file, File url]
	if ok then return ok else download us file
