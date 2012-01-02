{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUrl where

import Network.URI

import Common.Annex
import Command
import qualified Backend
import qualified Command.Add
import qualified Annex
import qualified Backend.URL
import Annex.Content
import Logs.Web

def :: [Command]
def = [command "addurl" (paramRepeating paramUrl) seek "add urls to annex"]

seek :: [CommandSeek]
seek = [withStrings start]

start :: String -> CommandStart
start s = notBareRepo $ go $ parseURI s
	where
		go Nothing = error $ "bad url " ++ s
		go (Just url) = do
			file <- liftIO $ url2file url
			showStart "addurl" file
			next $ perform s file

perform :: String -> FilePath -> CommandPerform
perform url file = do
	fast <- Annex.getState Annex.fast
	if fast then nodownload url file else download url file

download :: String -> FilePath -> CommandPerform
download url file = do
	showAction $ "downloading " ++ url ++ " "
	let dummykey = Backend.URL.fromUrl url
	tmp <- fromRepo $ gitAnnexTmpLocation dummykey
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	stopUnless (downloadUrl [url] tmp) $ do
		[(backend, _)] <- Backend.chooseBackends [file]
		k <- Backend.genKey tmp backend
		case k of
			Nothing -> stop
			Just (key, _) -> do
				moveAnnex key tmp
				setUrlPresent key url
				next $ Command.Add.cleanup file key True

nodownload :: String -> FilePath -> CommandPerform
nodownload url file = do
	let key = Backend.URL.fromUrl url
	setUrlPresent key url
	next $ Command.Add.cleanup file key False

url2file :: URI -> IO FilePath
url2file url = do
	whenM (doesFileExist file) $
		error $ "already have this url in " ++ file
	return file
	where
		file = escape $ uriRegName auth ++ uriPath url ++ uriQuery url
		escape = replace "/" "_" . replace "?" "_"
		auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
