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
import qualified Option

def :: [Command]
def = [withOptions [fileOption] $
	command "addurl" (paramRepeating paramUrl) seek "add urls to annex"]

fileOption :: Option
fileOption = Option.field [] "file" paramFile "specify what file the url is added to"

seek :: [CommandSeek]
seek = [withField fileOption return $ \f ->
	withStrings $ start f]

start :: Maybe FilePath -> String -> CommandStart
start optfile s = notBareRepo $ go $ parseURI s
	where
		go Nothing = error $ "bad url " ++ s
		go (Just url) = do
			let file = fromMaybe (url2file url) optfile
			showStart "addurl" file
			next $ perform s file

perform :: String -> FilePath -> CommandPerform
perform url file = ifAnnexed file addurl geturl
	where
		geturl = do
			whenM (liftIO $ doesFileExist file) $
				error $ "already have this url in " ++ file
			fast <- Annex.getState Annex.fast
			if fast then nodownload url file else download url file
		addurl (key, _backend) = do
			setUrlPresent key url
			next $ return True

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

url2file :: URI -> FilePath
url2file url = escape $ uriRegName auth ++ uriPath url ++ uriQuery url
	where
		escape = replace "/" "_" . replace "?" "_"
		auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
