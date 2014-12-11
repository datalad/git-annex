{- git-annex command
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.AddUrl where

import Network.URI

import Common.Annex
import Command
import Backend
import qualified Command.Add
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Url as Url
import qualified Backend.URL
import qualified Remote
import qualified Types.Remote as Remote
import Annex.URLClaim
import Annex.Content
import Logs.Web
import Types.Key
import Types.KeySource
import Config
import Annex.Content.Direct
import Logs.Location
import Utility.Metered
import qualified Annex.Transfer as Transfer
#ifdef WITH_QUVI
import Annex.Quvi
import qualified Utility.Quvi as Quvi
#endif

cmd :: [Command]
cmd = [notBareRepo $ withOptions [fileOption, pathdepthOption, relaxedOption] $
	command "addurl" (paramRepeating paramUrl) seek
		SectionCommon "add urls to annex"]

fileOption :: Option
fileOption = fieldOption [] "file" paramFile "specify what file the url is added to"

pathdepthOption :: Option
pathdepthOption = fieldOption [] "pathdepth" paramNumber "path components to use in filename"

relaxedOption :: Option
relaxedOption = flagOption [] "relaxed" "skip size check"

seek :: CommandSeek
seek ps = do
	f <- getOptionField fileOption return
	relaxed <- getOptionFlag relaxedOption
	d <- getOptionField pathdepthOption (return . maybe Nothing readish)
	withStrings (start relaxed f d) ps

start :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
start relaxed optfile pathdepth s = do
	(r, claim) <- urlClaim s
	if Remote.uuid r == webUUID
		then startWeb relaxed optfile pathdepth s
		else startRemote r claim relaxed optfile pathdepth s

startRemote :: Remote -> URLClaim -> Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
startRemote r claim relaxed optfile pathdepth s = do
	url <- case Url.parseURIRelaxed s of
		Nothing -> error $ "bad uri " ++ s
		Just u -> pure u
	pathmax <- liftIO $ fileNameLengthLimit "."
	let file = flip fromMaybe optfile $ case claim of
		URLClaimedAs f -> f
		URLClaimed -> url2file url pathdepth pathmax
	showStart "addurl" file
	showNote $ "using " ++ Remote.name r 
	next $ performRemote r relaxed s file

performRemote :: Remote -> Bool -> URLString -> FilePath -> CommandPerform
performRemote r relaxed uri file = ifAnnexed file adduri geturi
  where
	loguri = setDownloader uri OtherDownloader
	adduri = addUrlChecked relaxed loguri (Remote.uuid r) checkexistssize
	checkexistssize key = do
		res <- tryNonAsync $ Remote.checkUrl r uri
		case res of
			Left e -> do
				warning (show e)
				return (False, False)
			Right Nothing ->
				return (True, True)
			Right (Just sz) ->
				return (True, sz == fromMaybe sz (keySize key))
	geturi = do
		dummykey <- Backend.URL.fromUrl uri =<<
			if relaxed
				then return Nothing
				else Remote.checkUrl r uri
		liftIO $ createDirectoryIfMissing True (parentDir file)
		next $ ifM (Annex.getState Annex.fast <||> pure relaxed)
			( do
				res <- tryNonAsync $ Remote.checkUrl r uri
				case res of
					Left e -> do
						warning (show e)
						return False
					Right size -> do
						key <- Backend.URL.fromUrl uri size
						cleanup (Remote.uuid r) loguri file key Nothing
						return True
			, do
				-- Set temporary url for the dummy key
				-- so that the remote knows what url it
				-- should use to download it.
				setTempUrl dummykey uri
				let downloader = Remote.retrieveKeyFile r dummykey (Just file)
				ok <- isJust <$>
					downloadWith downloader dummykey (Remote.uuid r) loguri file
				removeTempUrl dummykey
				return ok
			)

startWeb :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
startWeb relaxed optfile pathdepth s = go $ fromMaybe bad $ parseURI s
  where
	(s', downloader) = getDownloader s
	bad = fromMaybe (error $ "bad url " ++ s') $
		parseURI $ escapeURIString isUnescapedInURI s'
	choosefile = flip fromMaybe optfile
	go url = case downloader of
		QuviDownloader -> usequvi
		_ -> 
#ifdef WITH_QUVI
			ifM (quviSupported s')
				( usequvi
				, regulardownload url
				)
#else
			regulardownload url
#endif
	regulardownload url = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = choosefile $ url2file url pathdepth pathmax
		showStart "addurl" file
		next $ performWeb relaxed s' file
#ifdef WITH_QUVI
	badquvi = error $ "quvi does not know how to download url " ++ s'
	usequvi = do
		page <- fromMaybe badquvi
			<$> withQuviOptions Quvi.forceQuery [Quvi.quiet, Quvi.httponly] s'
		let link = fromMaybe badquvi $ headMaybe $ Quvi.pageLinks page
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = choosefile $ truncateFilePath pathmax $ sanitizeFilePath $
			Quvi.pageTitle page ++ "." ++ Quvi.linkSuffix link
		showStart "addurl" file
		next $ performQuvi relaxed s' (Quvi.linkUrl link) file
#else
	usequvi = error "not built with quvi support"
#endif

#ifdef WITH_QUVI
performQuvi :: Bool -> URLString -> URLString -> FilePath -> CommandPerform
performQuvi relaxed pageurl videourl file = ifAnnexed file addurl geturl
  where
	quviurl = setDownloader pageurl QuviDownloader
	addurl key = next $ do
		cleanup webUUID quviurl file key Nothing
		return True
	geturl = next $ isJust <$> addUrlFileQuvi relaxed quviurl videourl file
#endif

#ifdef WITH_QUVI
addUrlFileQuvi :: Bool -> URLString -> URLString -> FilePath -> Annex (Maybe Key)
addUrlFileQuvi relaxed quviurl videourl file = do
	key <- Backend.URL.fromUrl quviurl Nothing
	ifM (pure relaxed <||> Annex.getState Annex.fast)
		( do
			cleanup webUUID quviurl file key Nothing
			return (Just key)
		, do
			{- Get the size, and use that to check
			 - disk space. However, the size info is not
			 - retained, because the size of a video stream
			 - might change and we want to be able to download
			 - it later. -}
			sizedkey <- addSizeUrlKey videourl key
			prepGetViaTmpChecked sizedkey Nothing $ do
				tmp <- fromRepo $ gitAnnexTmpObjectLocation key
				showOutput
				ok <- Transfer.notifyTransfer Transfer.Download (Just file) $
					Transfer.download webUUID key (Just file) Transfer.forwardRetry $ const $ do
						liftIO $ createDirectoryIfMissing True (parentDir tmp)
						downloadUrl [videourl] tmp
				if ok
					then do
						cleanup webUUID quviurl file key (Just tmp)
						return (Just key)
					else return Nothing
		)
#endif

performWeb :: Bool -> URLString -> FilePath -> CommandPerform
performWeb relaxed url file = ifAnnexed file addurl geturl
  where
	geturl = next $ isJust <$> addUrlFile relaxed url file
	addurl = addUrlChecked relaxed url webUUID checkexistssize
	checkexistssize = Url.withUrlOptions . Url.check url . keySize

addUrlChecked :: Bool -> URLString -> UUID -> (Key -> Annex (Bool, Bool)) -> Key -> CommandPerform
addUrlChecked relaxed url u checkexistssize key
	| relaxed = do
		setUrlPresent u key url
		next $ return True
	| otherwise = ifM (elem url <$> getUrls key)
		( stop
		, do
			(exists, samesize) <- checkexistssize key
			if exists && samesize
				then do
					setUrlPresent u key url
					next $ return True
				else do
					warning $ "while adding a new url to an already annexed file, " ++ if exists
						then "url does not have expected file size (use --relaxed to bypass this check) " ++ url
						else "failed to verify url exists: " ++ url
					stop
		)

addUrlFile :: Bool -> URLString -> FilePath -> Annex (Maybe Key)
addUrlFile relaxed url file = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure relaxed)
		( nodownload relaxed url file
		, downloadWeb url file
		)

downloadWeb :: URLString -> FilePath -> Annex (Maybe Key)
downloadWeb url file = do
	dummykey <- addSizeUrlKey url =<< Backend.URL.fromUrl url Nothing
	let downloader f _ = do
		showOutput
		downloadUrl [url] f
	showAction $ "downloading " ++ url ++ " "
	downloadWith downloader dummykey webUUID url file

{- The Key should be a dummy key, based on the URL, which is used
 - for this download, before we can examine the file and find its real key.
 - For resuming downloads to work, the dummy key for a given url should be
 - stable. -}
downloadWith :: (FilePath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> FilePath -> Annex (Maybe Key)
downloadWith downloader dummykey u url file =
	prepGetViaTmpChecked dummykey Nothing $ do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation dummykey
		ifM (runtransfer tmp)
			( do
				backend <- chooseBackend file
				let source = KeySource
					{ keyFilename = file
					, contentLocation = tmp
					, inodeCache = Nothing
					}
				k <- genKey source backend
				case k of
					Nothing -> return Nothing
					Just (key, _) -> do
						cleanup u url file key (Just tmp)
						return (Just key)
			, return Nothing
			)
  where
	runtransfer tmp =  Transfer.notifyTransfer Transfer.Download (Just file) $
		Transfer.download webUUID dummykey (Just file) Transfer.forwardRetry $ \p -> do
			liftIO $ createDirectoryIfMissing True (parentDir tmp)
			downloader tmp p

{- Hits the url to get the size, if available.
 -
 - This is needed to avoid exceeding the diskreserve when downloading,
 - and so the assistant can display a pretty progress bar.
 -}
addSizeUrlKey :: URLString -> Key -> Annex Key
addSizeUrlKey url key = do
	size <- snd <$> Url.withUrlOptions (Url.exists url)
	return $ key { keySize = size }

cleanup :: UUID -> URLString -> FilePath -> Key -> Maybe FilePath -> Annex ()
cleanup u url file key mtmp = do
	when (isJust mtmp) $
		logStatus key InfoPresent
	setUrlPresent u key url
	Command.Add.addLink file key Nothing
	whenM isDirect $ do
		void $ addAssociatedFile key file
		{- For moveAnnex to work in direct mode, the symlink
		 - must already exist, so flush the queue. -}
		Annex.Queue.flush
	maybe noop (moveAnnex key) mtmp

nodownload :: Bool -> URLString -> FilePath -> Annex (Maybe Key)
nodownload relaxed url file = do
	(exists, size) <- if relaxed
		then pure (True, Nothing)
		else Url.withUrlOptions (Url.exists url)
	if exists
		then do
			key <- Backend.URL.fromUrl url size
			cleanup webUUID url file key Nothing
			return (Just key)
		else do
			warning $ "unable to access url: " ++ url
			return Nothing

url2file :: URI -> Maybe Int -> Int -> FilePath
url2file url pathdepth pathmax = case pathdepth of
	Nothing -> truncateFilePath pathmax $ sanitizeFilePath fullurl
	Just depth
		| depth >= length urlbits -> frombits id
		| depth > 0 -> frombits $ drop depth
		| depth < 0 -> frombits $ reverse . take (negate depth) . reverse
		| otherwise -> error "bad --pathdepth"
  where
	fullurl = concat
		[ maybe "" uriRegName (uriAuthority url)
		, uriPath url
		, uriQuery url
		]
	frombits a = intercalate "/" $ a urlbits
	urlbits = map (truncateFilePath pathmax . sanitizeFilePath) $
		filter (not . null) $ split "/" fullurl
