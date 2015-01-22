{- git-annex command
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
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
import Annex.Content
import Annex.UUID
import Logs.Web
import Types.Key
import Types.KeySource
import Types.UrlContents
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
seek us = do
	optfile <- getOptionField fileOption return
	relaxed <- getOptionFlag relaxedOption
	pathdepth <- getOptionField pathdepthOption (return . maybe Nothing readish)
	forM_ us $ \u -> do
		r <- Remote.claimingUrl u
		if Remote.uuid r == webUUID
			then void $ commandAction $ startWeb relaxed optfile pathdepth u
			else do
				pathmax <- liftIO $ fileNameLengthLimit "."
				let deffile = fromMaybe (urlString2file u pathdepth pathmax) optfile
				res <- tryNonAsync $ maybe
					(error $ "unable to checkUrl of " ++ Remote.name r)
					(flip id u)
					(Remote.checkUrl r)
				case res of
					Left e -> void $ commandAction $ do
						showStart "addurl" u
						warning (show e)
						next $ next $ return False
					Right (UrlContents sz mf) -> do
						void $ commandAction $
							startRemote r relaxed (maybe deffile fromSafeFilePath mf) u sz
					Right (UrlMulti l) ->
						forM_ l $ \(u', sz, f) ->
							void $ commandAction $
								startRemote r relaxed (deffile </> fromSafeFilePath f) u' sz

startRemote :: Remote -> Bool -> FilePath -> URLString -> Maybe Integer -> CommandStart
startRemote r relaxed file uri sz = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let file' = joinPath $ map (truncateFilePath pathmax) $ splitDirectories file
	showStart "addurl" file'
	showNote $ "from " ++ Remote.name r 
	next $ performRemote r relaxed uri file' sz

performRemote :: Remote -> Bool -> URLString -> FilePath -> Maybe Integer -> CommandPerform
performRemote r relaxed uri file sz = ifAnnexed file adduri geturi
  where
	loguri = setDownloader uri OtherDownloader
	adduri = addUrlChecked relaxed loguri (Remote.uuid r) checkexistssize
	checkexistssize key = return $ case sz of
		Nothing -> (True, True)
		Just n -> (True, n == fromMaybe n (keySize key))
	geturi = next $ isJust <$> downloadRemoteFile r relaxed uri file sz

downloadRemoteFile :: Remote -> Bool -> URLString -> FilePath -> Maybe Integer -> Annex (Maybe Key)
downloadRemoteFile r relaxed uri file sz = do
	urlkey <- Backend.URL.fromUrl uri sz
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure relaxed)
		( do
			cleanup (Remote.uuid r) loguri file urlkey Nothing
			return (Just urlkey)
		, do
			-- Set temporary url for the urlkey
			-- so that the remote knows what url it
			-- should use to download it.
			setTempUrl urlkey loguri
			let downloader = Remote.retrieveKeyFile r urlkey (Just file)
			ret <- downloadWith downloader urlkey (Remote.uuid r) loguri file
			removeTempUrl urlkey
			return ret
		)
  where
	loguri = setDownloader uri OtherDownloader

startWeb :: Bool -> Maybe FilePath -> Maybe Int -> String -> CommandStart
startWeb relaxed optfile pathdepth s = go $ fromMaybe bad $ parseURI urlstring
  where
	(urlstring, downloader) = getDownloader s
	bad = fromMaybe (error $ "bad url " ++ urlstring) $
		parseURI $ escapeURIString isUnescapedInURI urlstring
	go url = case downloader of
		QuviDownloader -> usequvi
		_ -> 
#ifdef WITH_QUVI
			ifM (quviSupported urlstring)
				( usequvi
				, regulardownload url
				)
#else
			regulardownload url
#endif
	regulardownload url = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		urlinfo <- if relaxed
			then pure $ Url.UrlInfo True Nothing Nothing
			else Url.withUrlOptions (Url.getUrlInfo urlstring)
		file <- case optfile of
			Just f -> pure f
			Nothing -> case Url.urlSuggestedFile urlinfo of
				Nothing -> pure $ url2file url pathdepth pathmax
				Just sf -> do
					let f = truncateFilePath pathmax $
						sanitizeFilePath sf
					ifM (liftIO $ doesFileExist f <||> doesDirectoryExist f)
						( pure $ url2file url pathdepth pathmax
						, pure f
						)
		showStart "addurl" file
		next $ performWeb relaxed urlstring file urlinfo
#ifdef WITH_QUVI
	badquvi = error $ "quvi does not know how to download url " ++ urlstring
	usequvi = do
		page <- fromMaybe badquvi
			<$> withQuviOptions Quvi.forceQuery [Quvi.quiet, Quvi.httponly] urlstring
		let link = fromMaybe badquvi $ headMaybe $ Quvi.pageLinks page
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = flip fromMaybe optfile $
			truncateFilePath pathmax $ sanitizeFilePath $
				Quvi.pageTitle page ++ "." ++ Quvi.linkSuffix link
		showStart "addurl" file
		next $ performQuvi relaxed urlstring (Quvi.linkUrl link) file
#else
	usequvi = error "not built with quvi support"
#endif

performWeb :: Bool -> URLString -> FilePath -> Url.UrlInfo -> CommandPerform
performWeb relaxed url file urlinfo = ifAnnexed file addurl geturl
  where
	geturl = next $ isJust <$> addUrlFile relaxed url urlinfo file
	addurl = addUrlChecked relaxed url webUUID $ \k -> return $
		(Url.urlExists urlinfo, Url.urlSize urlinfo == keySize k)

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
			urlinfo <- Url.withUrlOptions (Url.getUrlInfo videourl)
			let sizedkey = addSizeUrlKey urlinfo key
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

addUrlChecked :: Bool -> URLString -> UUID -> (Key -> Annex (Bool, Bool)) -> Key -> CommandPerform
addUrlChecked relaxed url u checkexistssize key
	| relaxed = do
		setUrlPresent u key url
		next $ return True
	| otherwise = ifM ((elem url <$> getUrls key) <&&> (elem u <$> loggedLocations key))
		( next $ return True -- nothing to do
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

addUrlFile :: Bool -> URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
addUrlFile relaxed url urlinfo file = do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure relaxed)
		( nodownload relaxed url urlinfo file
		, downloadWeb url urlinfo file
		)

downloadWeb :: URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
downloadWeb url urlinfo file = do
	dummykey <- addSizeUrlKey urlinfo <$> Backend.URL.fromUrl url Nothing
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
		Transfer.download u dummykey (Just file) Transfer.forwardRetry $ \p -> do
			liftIO $ createDirectoryIfMissing True (parentDir tmp)
			downloader tmp p

{- Adds the url size to the Key. -}
addSizeUrlKey :: Url.UrlInfo -> Key -> Key
addSizeUrlKey urlinfo key = key { keySize = Url.urlSize urlinfo }

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

nodownload :: Bool -> URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
nodownload relaxed url urlinfo file
	| Url.urlExists urlinfo = do
		key <- Backend.URL.fromUrl url (Url.urlSize urlinfo)
		cleanup webUUID url file key Nothing
		return (Just key)
	| otherwise = do
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

urlString2file :: URLString -> Maybe Int -> Int -> FilePath
urlString2file s pathdepth pathmax = case Url.parseURIRelaxed s of
	Nothing -> error $ "bad uri " ++ s
	Just u -> url2file u pathdepth pathmax
