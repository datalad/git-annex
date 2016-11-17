{- git-annex command
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.AddUrl where

import Network.URI

import Command
import Backend
import qualified Annex
import qualified Annex.Url as Url
import qualified Backend.URL
import qualified Remote
import qualified Types.Remote as Remote
import qualified Command.Add
import Annex.Content
import Annex.Ingest
import Annex.CheckIgnore
import Annex.UUID
import Logs.Web
import Types.KeySource
import Types.UrlContents
import Annex.FileMatcher
import Logs.Location
import Utility.Metered
import qualified Annex.Transfer as Transfer
import Annex.Quvi
import qualified Utility.Quvi as Quvi

cmd :: Command
cmd = notBareRepo $ withGlobalOptions [jobsOption, jsonOption, jsonProgressOption] $
	command "addurl" SectionCommon "add urls to annex"
		(paramRepeating paramUrl) (seek <$$> optParser)

data AddUrlOptions = AddUrlOptions
	{ addUrls :: CmdParams
	, fileOption :: Maybe FilePath
	, pathdepthOption :: Maybe Int
	, prefixOption :: Maybe String
	, suffixOption :: Maybe String
	, relaxedOption :: Bool
	, rawOption :: Bool
	, batchOption :: BatchMode
	, batchFilesOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser AddUrlOptions
optParser desc = AddUrlOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "file" <> metavar paramFile
		<> help "specify what file the url is added to"
		))
	<*> optional (option auto
		( long "pathdepth" <> metavar paramNumber
		<> help "number of url path components to use in filename"
		))
	<*> optional (strOption
		( long "prefix" <> metavar paramValue
		<> help "add a prefix to the filename"
		))
	<*> optional (strOption
		( long "suffix" <> metavar paramValue
		<> help "add a suffix to the filename"
		))
	<*> parseRelaxedOption
	<*> parseRawOption
	<*> parseBatchOption
	<*> switch
		( long "with-files"
		<> help "parse batch mode lines of the form \"$url $file\""
		)

parseRelaxedOption :: Parser Bool
parseRelaxedOption = switch
	( long "relaxed"
	<> help "skip size check"
	)

parseRawOption :: Parser Bool
parseRawOption = switch
	( long "raw"
	<> help "disable special handling for torrents, quvi, etc"
	)

seek :: AddUrlOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	forM_ (addUrls o) (\u -> go (o, u))
	case batchOption o of
		Batch -> batchInput (parseBatchInput o) go
		NoBatch -> noop
  where
	go (o', u) = do
		r <- Remote.claimingUrl u
		if Remote.uuid r == webUUID || rawOption o'
			then void $ commandAction $ startWeb o' u
			else checkUrl r o' u

parseBatchInput :: AddUrlOptions -> String -> Either String (AddUrlOptions, URLString)
parseBatchInput o s
	| batchFilesOption o =
		let (u, f) = separate (== ' ') s
		in if null u || null f
			then Left ("parsed empty url or filename in input: " ++ s)
			else Right (o { fileOption = Just f }, u)
	| otherwise = Right (o, s)

checkUrl :: Remote -> AddUrlOptions -> URLString -> Annex ()
checkUrl r o u = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let deffile = fromMaybe (urlString2file u (pathdepthOption o) pathmax) (fileOption o)
	go deffile =<< maybe
		(error $ "unable to checkUrl of " ++ Remote.name r)
		(tryNonAsync . flip id u)
		(Remote.checkUrl r)
  where

	go _ (Left e) = void $ commandAction $ do
		showStart "addurl" u
		warning (show e)
		next $ next $ return False
	go deffile (Right (UrlContents sz mf)) = do
		let f = adjustFile o (fromMaybe (maybe deffile fromSafeFilePath mf) (fileOption o))
		void $ commandAction $
			startRemote r (relaxedOption o) f u sz
	go deffile (Right (UrlMulti l))
		| isNothing (fileOption o) =
			forM_ l $ \(u', sz, f) -> do
				let f' = adjustFile o (deffile </> fromSafeFilePath f)
				void $ commandAction $
					startRemote r (relaxedOption o) f' u' sz
		| otherwise = giveup $ unwords
			[ "That url contains multiple files according to the"
			, Remote.name r
			, " remote; cannot add it to a single file."
			]

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
downloadRemoteFile r relaxed uri file sz = checkCanAdd file $ do
	let urlkey = Backend.URL.fromUrl uri sz
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
			let downloader = \dest p -> fst <$> Remote.retrieveKeyFile r urlkey (Just file) dest p
			ret <- downloadWith downloader urlkey (Remote.uuid r) loguri file
			removeTempUrl urlkey
			return ret
		)
  where
	loguri = setDownloader uri OtherDownloader

startWeb :: AddUrlOptions -> String -> CommandStart
startWeb o s = go $ fromMaybe bad $ parseURI urlstring
  where
	(urlstring, downloader) = getDownloader s
	bad = fromMaybe (giveup $ "bad url " ++ urlstring) $
		Url.parseURIRelaxed $ urlstring
	go url = case downloader of
		QuviDownloader -> usequvi
		_ -> ifM (quviSupported urlstring)
			( usequvi
			, regulardownload url
			)
	regulardownload url = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		urlinfo <- if relaxedOption o
			then pure Url.assumeUrlExists
			else Url.withUrlOptions (Url.getUrlInfo urlstring)
		file <- adjustFile o <$> case fileOption o of
			Just f -> pure f
			Nothing -> case Url.urlSuggestedFile urlinfo of
				Nothing -> pure $ url2file url (pathdepthOption o) pathmax
				Just sf -> do
					let f = truncateFilePath pathmax $
						sanitizeFilePath sf
					ifM (liftIO $ doesFileExist f <||> doesDirectoryExist f)
						( pure $ url2file url (pathdepthOption o) pathmax
						, pure f
						)
		showStart "addurl" file
		next $ performWeb (relaxedOption o) urlstring file urlinfo
	badquvi = giveup $ "quvi does not know how to download url " ++ urlstring
	usequvi = do
		page <- fromMaybe badquvi
			<$> withQuviOptions Quvi.forceQuery [Quvi.quiet, Quvi.httponly] urlstring
		let link = fromMaybe badquvi $ headMaybe $ Quvi.pageLinks page
		pathmax <- liftIO $ fileNameLengthLimit "."
		let file = adjustFile o $ flip fromMaybe (fileOption o) $
			truncateFilePath pathmax $ sanitizeFilePath $
				Quvi.pageTitle page ++ "." ++ fromMaybe "m" (Quvi.linkSuffix link)
		showStart "addurl" file
		next $ performQuvi (relaxedOption o) urlstring (Quvi.linkUrl link) file

performWeb :: Bool -> URLString -> FilePath -> Url.UrlInfo -> CommandPerform
performWeb relaxed url file urlinfo = ifAnnexed file addurl geturl
  where
	geturl = next $ isJust <$> addUrlFile relaxed url urlinfo file
	addurl = addUrlChecked relaxed url webUUID $ \k -> return $
		(Url.urlExists urlinfo, Url.urlSize urlinfo == keySize k)

performQuvi :: Bool -> URLString -> URLString -> FilePath -> CommandPerform
performQuvi relaxed pageurl videourl file = ifAnnexed file addurl geturl
  where
	quviurl = setDownloader pageurl QuviDownloader
	addurl key = next $ do
		cleanup webUUID quviurl file key Nothing
		return True
	geturl = next $ isJust <$> addUrlFileQuvi relaxed quviurl videourl file

addUrlFileQuvi :: Bool -> URLString -> URLString -> FilePath -> Annex (Maybe Key)
addUrlFileQuvi relaxed quviurl videourl file = checkCanAdd file $ do
	let key = Backend.URL.fromUrl quviurl Nothing
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
			checkDiskSpaceToGet sizedkey Nothing $ do
				tmp <- fromRepo $ gitAnnexTmpObjectLocation key
				showOutput
				ok <- Transfer.notifyTransfer Transfer.Download (Just file) $
					Transfer.download webUUID key (Just file) Transfer.forwardRetry $ \p -> do
						liftIO $ createDirectoryIfMissing True (parentDir tmp)
						downloadUrl key p [videourl] tmp
				if ok
					then do
						cleanup webUUID quviurl file key (Just tmp)
						return (Just key)
					else return Nothing
		)

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
addUrlFile relaxed url urlinfo file = checkCanAdd file $ do
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure relaxed)
		( nodownload url urlinfo file
		, downloadWeb url urlinfo file
		)

downloadWeb :: URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
downloadWeb url urlinfo file = do
	let dummykey = addSizeUrlKey urlinfo $ Backend.URL.fromUrl url Nothing
	let downloader f p = do
		showOutput
		downloadUrl dummykey p [url] f
	showAction $ "downloading " ++ url ++ " "
	downloadWith downloader dummykey webUUID url file

{- The Key should be a dummy key, based on the URL, which is used
 - for this download, before we can examine the file and find its real key.
 - For resuming downloads to work, the dummy key for a given url should be
 - stable. -}
downloadWith :: (FilePath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> FilePath -> Annex (Maybe Key)
downloadWith downloader dummykey u url file =
	checkDiskSpaceToGet dummykey Nothing $ do
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
cleanup u url file key mtmp = case mtmp of
	Nothing -> go
	Just tmp -> do
		largematcher <- largeFilesMatcher
		ifM (checkFileMatcher largematcher file)
			( go
			, do
				liftIO $ renameFile tmp file
				void $ Command.Add.addSmall file
			)
  where
	go = do
		maybeShowJSON $ JSONChunk [("key", key2file key)]
		when (isJust mtmp) $
			logStatus key InfoPresent
		setUrlPresent u key url
		addAnnexedFile file key mtmp

nodownload :: URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
nodownload url urlinfo file
	| Url.urlExists urlinfo = do
		let key = Backend.URL.fromUrl url (Url.urlSize urlinfo)
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
		| otherwise -> giveup "bad --pathdepth"
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
	Nothing -> giveup $ "bad uri " ++ s
	Just u -> url2file u pathdepth pathmax

adjustFile :: AddUrlOptions -> FilePath -> FilePath
adjustFile o = addprefix . addsuffix
  where
	addprefix f = maybe f (++ f) (prefixOption o)
	addsuffix f = maybe f (f ++) (suffixOption o)

checkCanAdd :: FilePath -> Annex (Maybe a) -> Annex (Maybe a)
checkCanAdd file a = ifM (isJust <$> (liftIO $ catchMaybeIO $ getSymbolicLinkStatus file))
	( do
		warning $ file ++ " already exists and is not annexed; not overwriting"
		return Nothing
	, ifM ((not <$> Annex.getState Annex.force) <&&> checkIgnored file)
		( do
			warning $ "not adding " ++ file ++ " which is .gitignored (use --force to override)"
			return Nothing
		, a
		)
	)
