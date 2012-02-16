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
import qualified Utility.Url as Url
import Annex.Content
import Logs.Web
import qualified Option
import Types.Key

def :: [Command]
def = [withOptions [fileOption, pathdepthOption] $
	command "addurl" (paramRepeating paramUrl) seek "add urls to annex"]

fileOption :: Option
fileOption = Option.field [] "file" paramFile "specify what file the url is added to"

pathdepthOption :: Option
pathdepthOption = Option.field [] "pathdepth" paramFile "number of path components to use in filename"

seek :: [CommandSeek]
seek = [withField fileOption return $ \f ->
	withField pathdepthOption (return . maybe Nothing readish) $ \d ->
	withStrings $ start f d]

start :: Maybe FilePath -> Maybe Int -> String -> CommandStart
start optfile pathdepth s = notBareRepo $ go $ fromMaybe bad $ parseURI s
	where
		bad = fromMaybe (error $ "bad url " ++ s) $
			parseURI $ escapeURIString isUnescapedInURI s
		go url = do
			let file = fromMaybe (url2file url pathdepth) optfile
			showStart "addurl" file
			next $ perform s file pathdepth

perform :: String -> FilePath -> Maybe Int -> CommandPerform
perform url file pathdepth = ifAnnexed file addurl geturl
	where
		geturl = do
			liftIO $ createDirectoryIfMissing True (parentDir file)
			fast <- Annex.getState Annex.fast
			if fast then nodownload url file else download url file
		addurl (key, _backend) = do
			when (pathdepth /= Nothing) $
				error $ file ++ " already exists"
			unlessM (liftIO $ Url.check url (keySize key)) $
				error $ "failed to verify url: " ++ url
			setUrlPresent key url
			next $ return True

download :: String -> FilePath -> CommandPerform
download url file = do
	showAction $ "downloading " ++ url ++ " "
	let dummykey = Backend.URL.fromUrl url Nothing
	tmp <- fromRepo $ gitAnnexTmpLocation dummykey
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	stopUnless (downloadUrl [url] tmp) $ do
		backend <- Backend.chooseBackend file
		k <- Backend.genKey tmp backend
		case k of
			Nothing -> stop
			Just (key, _) -> do
				moveAnnex key tmp
				setUrlPresent key url
				next $ Command.Add.cleanup file key True

nodownload :: String -> FilePath -> CommandPerform
nodownload url file = do
	(exists, size) <- liftIO $ Url.exists url
	unless exists $
		error $ "unable to access url: " ++ url
	let key = Backend.URL.fromUrl url size
	setUrlPresent key url
	next $ Command.Add.cleanup file key False

url2file :: URI -> Maybe Int -> FilePath
url2file url pathdepth = case pathdepth of
	Nothing -> filesize $ escape fullurl
	Just depth
		| depth > 0 -> filesize $ join "/" $
			fromend depth $ map escape $
			filter (not . null) $ split "/" fullurl
		| otherwise -> error "bad --pathdepth value"
	where
		fullurl = uriRegName auth ++ uriPath url ++ uriQuery url
		auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
		filesize = take 255
		escape = replace "/" "_" . replace "?" "_"
		fromend n = reverse . take n . reverse
