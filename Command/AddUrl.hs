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
import Backend
import qualified Command.Add
import qualified Annex
import qualified Annex.Queue
import qualified Backend.URL
import qualified Utility.Url as Url
import Annex.Content
import Logs.Web
import qualified Option
import Types.Key
import Types.KeySource
import Config
import Annex.Content.Direct
import Logs.Location
import qualified Logs.Transfer as Transfer
import Utility.Daemon (checkDaemon)

def :: [Command]
def = [notBareRepo $ withOptions [fileOption, pathdepthOption, relaxedOption] $
	command "addurl" (paramRepeating paramUrl) seek
		SectionCommon "add urls to annex"]

fileOption :: Option
fileOption = Option.field [] "file" paramFile "specify what file the url is added to"

pathdepthOption :: Option
pathdepthOption = Option.field [] "pathdepth" paramNumber "path components to use in filename"

relaxedOption :: Option
relaxedOption = Option.flag [] "relaxed" "skip size check"

seek :: [CommandSeek]
seek = [withField fileOption return $ \f ->
	withFlag relaxedOption $ \relaxed ->
	withField pathdepthOption (return . maybe Nothing readish) $ \d ->
	withStrings $ start relaxed f d]

start :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
start relaxed optfile pathdepth s = go $ fromMaybe bad $ parseURI s
  where
	bad = fromMaybe (error $ "bad url " ++ s) $
		parseURI $ escapeURIString isUnescapedInURI s
	go url = do
		let file = fromMaybe (url2file url pathdepth) optfile
		showStart "addurl" file
		next $ perform relaxed s file

perform :: Bool -> String -> FilePath -> CommandPerform
perform relaxed url file = ifAnnexed file addurl geturl
  where
	geturl = do
		liftIO $ createDirectoryIfMissing True (parentDir file)
		ifM (Annex.getState Annex.fast <||> pure relaxed)
			( nodownload relaxed url file , download url file )
	addurl (key, _backend)
		| relaxed = do
			setUrlPresent key url
			next $ return True
		| otherwise = do
			headers <- getHttpHeaders
			ifM (liftIO $ Url.check url headers $ keySize key)
				( do
					setUrlPresent key url
					next $ return True
				, do
					warning $ "failed to verify url: " ++ url
					stop
				)

download :: String -> FilePath -> CommandPerform
download url file = do
	showAction $ "downloading " ++ url ++ " "
	dummykey <- genkey
	tmp <- fromRepo $ gitAnnexTmpLocation dummykey
	stopUnless (runtransfer dummykey tmp) $ do
		backend <- chooseBackend file
		let source = KeySource
			{ keyFilename = file
			, contentLocation = tmp
			, inodeCache = Nothing
			}
		k <- genKey source backend
		case k of
			Nothing -> stop
			Just (key, _) -> next $ cleanup url file key (Just tmp)
  where
	{- Generate a dummy key to use for this download, before we can
	 - examine the file and find its real key. This allows resuming
	 - downloads, as the dummy key for a given url is stable.
	 -
	 - If the assistant is running, actually hits the url here,
	 - to get the size, so it can display a pretty progress bar.
	 -}
	genkey = do
		pidfile <- fromRepo gitAnnexPidFile
		size <- ifM (liftIO $ isJust <$> checkDaemon pidfile)
			( do
				headers <- getHttpHeaders
				liftIO $ snd <$> Url.exists url headers
			, return Nothing
			)
		return $ Backend.URL.fromUrl url size
  	runtransfer dummykey tmp = 
		Transfer.download webUUID dummykey (Just file) Transfer.forwardRetry $ do
			liftIO $ createDirectoryIfMissing True (parentDir tmp)
			downloadUrl [url] tmp
		

cleanup :: String -> FilePath -> Key -> Maybe FilePath -> CommandCleanup
cleanup url file key mtmp = do
	when (isJust mtmp) $
		logStatus key InfoPresent
	setUrlPresent key url
	Command.Add.addLink file key False
	whenM isDirect $ do
		void $ addAssociatedFile key file
		{- For moveAnnex to work in direct mode, the symlink
		 - must already exist, so flush the queue. -}
		Annex.Queue.flush
	maybe noop (moveAnnex key) mtmp
	return True

nodownload :: Bool -> String -> FilePath -> CommandPerform
nodownload relaxed url file = do
	headers <- getHttpHeaders
	(exists, size) <- if relaxed
		then pure (True, Nothing)
		else liftIO $ Url.exists url headers
	if exists
		then do
			let key = Backend.URL.fromUrl url size
			next $ cleanup url file key Nothing
		else do
			warning $ "unable to access url: " ++ url
			stop

url2file :: URI -> Maybe Int -> FilePath
url2file url pathdepth = case pathdepth of
	Nothing -> filesize $ escape fullurl
	Just depth
		| depth > 0 -> frombits $ drop depth
		| depth < 0 -> frombits $ reverse . take (negate depth) . reverse
		| otherwise -> error "bad --pathdepth"
  where
	fullurl = uriRegName auth ++ uriPath url ++ uriQuery url
	frombits a = join "/" $ a urlbits
	urlbits = map (filesize . escape) $ filter (not . null) $ split "/" fullurl
	auth = fromMaybe (error $ "bad url " ++ show url) $ uriAuthority url
	filesize = take 255
	escape = replace "/" "_" . replace "?" "_"
