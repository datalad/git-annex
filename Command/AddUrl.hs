{- git-annex command
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Annex.YoutubeDl
import Logs.Web
import Types.KeySource
import Types.UrlContents
import Annex.FileMatcher
import Logs.Location
import Utility.Metered
import Utility.HtmlDetect
import Utility.Path.Max
import qualified Annex.Transfer as Transfer

cmd :: Command
cmd = notBareRepo $ withGlobalOptions [jobsOption, jsonOptions, jsonProgressOption] $
	command "addurl" SectionCommon "add urls to annex"
		(paramRepeating paramUrl) (seek <$$> optParser)

data AddUrlOptions = AddUrlOptions
	{ addUrls :: CmdParams
	, pathdepthOption :: Maybe Int
	, prefixOption :: Maybe String
	, suffixOption :: Maybe String
	, downloadOptions :: DownloadOptions
	, batchOption :: BatchMode
	, batchFilesOption :: Bool
	}

data DownloadOptions = DownloadOptions
	{ relaxedOption :: Bool
	, rawOption :: Bool
	, fileOption :: Maybe FilePath
	}

optParser :: CmdParamsDesc -> Parser AddUrlOptions
optParser desc = AddUrlOptions
	<$> cmdParams desc
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
	<*> parseDownloadOptions True
	<*> parseBatchOption
	<*> switch
		( long "with-files"
		<> help "parse batch mode lines of the form \"$url $file\""
		)

parseDownloadOptions :: Bool -> Parser DownloadOptions
parseDownloadOptions withfileoption = DownloadOptions
	<$> switch
		( long "relaxed"
		<> help "skip size check"
		)
	<*> switch
		( long "raw"
		<> help "disable special handling for torrents, youtube-dl, etc"
		)
	<*> if withfileoption
		then optional (strOption
			( long "file" <> metavar paramFile
			<> help "specify what file the url is added to"
			))
		else pure Nothing

seek :: AddUrlOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	forM_ (addUrls o) (\u -> go (o, u))
	case batchOption o of
		Batch fmt -> batchInput fmt (parseBatchInput o) go
		NoBatch -> noop
  where
	go (o', u) = do
		r <- Remote.claimingUrl u
		if Remote.uuid r == webUUID || rawOption (downloadOptions o')
			then void $ commandAction $ startWeb o' u
			else checkUrl r o' u

parseBatchInput :: AddUrlOptions -> String -> Either String (AddUrlOptions, URLString)
parseBatchInput o s
	| batchFilesOption o =
		let (u, f) = separate (== ' ') s
		in if null u || null f
			then Left ("parsed empty url or filename in input: " ++ s)
			else Right (o { downloadOptions = (downloadOptions o) { fileOption = Just f } }, u)
	| otherwise = Right (o, s)

checkUrl :: Remote -> AddUrlOptions -> URLString -> Annex ()
checkUrl r o u = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let deffile = fromMaybe (urlString2file u (pathdepthOption o) pathmax) (fileOption (downloadOptions o))
	go deffile =<< maybe
		(error $ "unable to checkUrl of " ++ Remote.name r)
		(tryNonAsync . flip id u)
		(Remote.checkUrl r)
  where

	go _ (Left e) = void $ commandAction $ startingAddUrl u o $ do
		warning (show e)
		next $ return False
	go deffile (Right (UrlContents sz mf)) = do
		let f = adjustFile o (fromMaybe (maybe deffile fromSafeFilePath mf) (fileOption (downloadOptions o)))
		void $ commandAction $ startRemote r o f u sz
	go deffile (Right (UrlMulti l)) = case fileOption (downloadOptions o) of
		Nothing ->
			forM_ l $ \(u', sz, f) -> do
				let f' = adjustFile o (deffile </> fromSafeFilePath f)
				void $ commandAction $ startRemote r o f' u' sz
		Just f -> case l of
			[] -> noop
			((u',sz,_):[]) -> do
				let f' = adjustFile o f
				void $ commandAction $ startRemote r o f' u' sz
			_ -> giveup $ unwords
				[ "That url contains multiple files according to the"
				, Remote.name r
				, " remote; cannot add it to a single file."
				]

startRemote :: Remote -> AddUrlOptions -> FilePath -> URLString -> Maybe Integer -> CommandStart
startRemote r o file uri sz = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let file' = joinPath $ map (truncateFilePath pathmax) $ splitDirectories file
	startingAddUrl uri o $ do
		showNote $ "from " ++ Remote.name r 
		showDestinationFile file'
		performRemote r o uri file' sz

performRemote :: Remote -> AddUrlOptions -> URLString -> FilePath -> Maybe Integer -> CommandPerform
performRemote r o uri file sz = ifAnnexed file adduri geturi
  where
	loguri = setDownloader uri OtherDownloader
	adduri = addUrlChecked o loguri file (Remote.uuid r) checkexistssize
	checkexistssize key = return $ case sz of
		Nothing -> (True, True, loguri)
		Just n -> (True, n == fromMaybe n (keySize key), loguri)
	geturi = next $ isJust <$> downloadRemoteFile r (downloadOptions o) uri file sz

downloadRemoteFile :: Remote -> DownloadOptions -> URLString -> FilePath -> Maybe Integer -> Annex (Maybe Key)
downloadRemoteFile r o uri file sz = checkCanAdd file $ do
	let urlkey = Backend.URL.fromUrl uri sz
	liftIO $ createDirectoryIfMissing True (parentDir file)
	ifM (Annex.getState Annex.fast <||> pure (relaxedOption o))
		( do
			addWorkTree (Remote.uuid r) loguri file urlkey Nothing
			return (Just urlkey)
		, do
			-- Set temporary url for the urlkey
			-- so that the remote knows what url it
			-- should use to download it.
			setTempUrl urlkey loguri
			let downloader = \dest p -> fst 
				<$> Remote.retrieveKeyFile r urlkey
					(AssociatedFile (Just file)) dest p
			ret <- downloadWith downloader urlkey (Remote.uuid r) loguri file
			removeTempUrl urlkey
			return ret
		)
  where
	loguri = setDownloader uri OtherDownloader

startWeb :: AddUrlOptions -> URLString -> CommandStart
startWeb o urlstring = go $ fromMaybe bad $ parseURI urlstring
  where
	bad = fromMaybe (giveup $ "bad url " ++ urlstring) $
		Url.parseURIRelaxed $ urlstring
	go url = startingAddUrl urlstring o $ do
		pathmax <- liftIO $ fileNameLengthLimit "."
		urlinfo <- if relaxedOption (downloadOptions o)
			then pure Url.assumeUrlExists
			else Url.withUrlOptions $
				liftIO . Url.getUrlInfo urlstring
		file <- adjustFile o <$> case fileOption (downloadOptions o) of
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
		performWeb o urlstring file urlinfo

performWeb :: AddUrlOptions -> URLString -> FilePath -> Url.UrlInfo -> CommandPerform
performWeb o url file urlinfo = ifAnnexed file addurl geturl
  where
	geturl = next $ isJust <$> addUrlFile (downloadOptions o) url urlinfo file
	addurl = addUrlChecked o url file webUUID $ \k ->
		ifM (pure (not (rawOption (downloadOptions o))) <&&> youtubeDlSupported url)
			( return (True, True, setDownloader url YoutubeDownloader)
			, return (Url.urlExists urlinfo, Url.urlSize urlinfo == keySize k, url)
			)

{- Check that the url exists, and has the same size as the key,
 - and add it as an url to the key. -}
addUrlChecked :: AddUrlOptions -> URLString -> FilePath -> UUID -> (Key -> Annex (Bool, Bool, URLString)) -> Key -> CommandPerform
addUrlChecked o url file u checkexistssize key =
	ifM ((elem url <$> getUrls key) <&&> (elem u <$> loggedLocations key))
		( do
			showDestinationFile file
			next $ return True
		, do
			(exists, samesize, url') <- checkexistssize key
			if exists && (samesize || relaxedOption (downloadOptions o))
				then do
					setUrlPresent key url'
					logChange key u InfoPresent
					next $ return True
				else do
					warning $ "while adding a new url to an already annexed file, " ++ if exists
						then "url does not have expected file size (use --relaxed to bypass this check) " ++ url
						else "failed to verify url exists: " ++ url
					stop
		)

{- Downloads an url (except in fast or relaxed mode) and adds it to the
 - repository, normally at the specified FilePath. 
 - But, if youtube-dl supports the url, it will be written to a
 - different file, based on the title of the media. Unless the user
 - specified fileOption, which then forces using the FilePath.
 -}
addUrlFile :: DownloadOptions -> URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
addUrlFile o url urlinfo file =
	ifM (Annex.getState Annex.fast <||> pure (relaxedOption o))
		( nodownloadWeb o url urlinfo file
		, downloadWeb o url urlinfo file
		)

downloadWeb :: DownloadOptions -> URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
downloadWeb o url urlinfo file =
	go =<< downloadWith' downloader urlkey webUUID url (AssociatedFile (Just file))
  where
	urlkey = addSizeUrlKey urlinfo $ Backend.URL.fromUrl url Nothing
	downloader f p = downloadUrl urlkey p [url] f
	go Nothing = return Nothing
	-- If we downloaded a html file, try to use youtube-dl to
	-- extract embedded media.
	go (Just tmp) = ifM (pure (not (rawOption o)) <&&> liftIO (isHtml <$> readFile tmp))
		( tryyoutubedl tmp
		, normalfinish tmp
		)
	normalfinish tmp = checkCanAdd file $ do
		showDestinationFile file
		liftIO $ createDirectoryIfMissing True (parentDir file)
		finishDownloadWith tmp webUUID url file
	tryyoutubedl tmp
		-- Ask youtube-dl what filename it will download
		-- first, and check if that is already an annexed file,
		-- to avoid unnecessary work in that case.
		| otherwise = youtubeDlFileNameHtmlOnly url >>= \case
			Right dest -> ifAnnexed dest 
				(alreadyannexed dest)
				(dl dest)
			Left _ -> normalfinish tmp
		-- Ask youtube-dl what filename it will download
		-- fist, so it's only used when the file contains embedded
		-- media.
		| isJust (fileOption o) = youtubeDlFileNameHtmlOnly url >>= \case
			Right _ -> dl file
			Left _ -> normalfinish tmp
	  where
		dl dest = withTmpWorkDir mediakey $ \workdir -> do
			let cleanuptmp = pruneTmpWorkDirBefore tmp (liftIO . nukeFile)
			Transfer.notifyTransfer Transfer.Download url $
				Transfer.download webUUID mediakey (AssociatedFile Nothing) Transfer.noRetry $ \_p ->
					youtubeDl url workdir >>= \case
						Right (Just mediafile) -> do
							cleanuptmp
							checkCanAdd dest $ do
								showDestinationFile dest
								addWorkTree webUUID mediaurl dest mediakey (Just mediafile)
								return $ Just mediakey
						Right Nothing -> normalfinish tmp
						Left msg -> do
							cleanuptmp
							warning msg
							return Nothing
		mediaurl = setDownloader url YoutubeDownloader
		mediakey = Backend.URL.fromUrl mediaurl Nothing
		-- Does the already annexed file have the mediaurl
		-- as an url? If so nothing to do.
		alreadyannexed dest k = do
			us <- getUrls k
			if mediaurl `elem` us
				then return (Just k)
				else do
					warning $ dest ++ " already exists; not overwriting"
					return Nothing

{- The destination file is not known at start time unless the user provided
 - a filename. It's not displayed then for output consistency, 
 - but is added to the json when available. -}
startingAddUrl :: URLString -> AddUrlOptions -> CommandPerform -> CommandStart
startingAddUrl url o p = starting "addurl" (ActionItemOther (Just url)) $ do
	case fileOption (downloadOptions o) of
		Nothing -> noop
		Just file -> maybeShowJSON $ JSONChunk [("file", file)]
	p

showDestinationFile :: FilePath -> Annex ()
showDestinationFile file = do
	showNote ("to " ++ file)
	maybeShowJSON $ JSONChunk [("file", file)]

{- The Key should be a dummy key, based on the URL, which is used
 - for this download, before we can examine the file and find its real key.
 - For resuming downloads to work, the dummy key for a given url should be
 - stable. For disk space checking to work, the dummy key should have
 - the size of the url already set.
 -
 - Downloads the url, sets up the worktree file, and returns the
 - real key.
 -}
downloadWith :: (FilePath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> FilePath -> Annex (Maybe Key)
downloadWith downloader dummykey u url file =
	go =<< downloadWith' downloader dummykey u url afile
  where
	afile = AssociatedFile (Just file)
	go Nothing = return Nothing
	go (Just tmp) = finishDownloadWith tmp u url file

{- Like downloadWith, but leaves the dummy key content in
 - the returned location. -}
downloadWith' :: (FilePath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> AssociatedFile -> Annex (Maybe FilePath)
downloadWith' downloader dummykey u url afile =
	checkDiskSpaceToGet dummykey Nothing $ do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation dummykey
		ok <- Transfer.notifyTransfer Transfer.Download url $
			Transfer.download u dummykey afile Transfer.stdRetry $ \p -> do
				liftIO $ createDirectoryIfMissing True (parentDir tmp)
				downloader tmp p
		if ok
			then return (Just tmp)
			else return Nothing

finishDownloadWith :: FilePath -> UUID -> URLString -> FilePath -> Annex (Maybe Key)
finishDownloadWith tmp u url file = do
	backend <- chooseBackend file
	let source = KeySource
		{ keyFilename = file
		, contentLocation = tmp
		, inodeCache = Nothing
		}
	genKey source nullMeterUpdate backend >>= \case
		Nothing -> return Nothing
		Just (key, _) -> do
			addWorkTree u url file key (Just tmp)
			return (Just key)

{- Adds the url size to the Key. -}
addSizeUrlKey :: Url.UrlInfo -> Key -> Key
addSizeUrlKey urlinfo key = key { keySize = Url.urlSize urlinfo }

{- Adds worktree file to the repository. -}
addWorkTree :: UUID -> URLString -> FilePath -> Key -> Maybe FilePath -> Annex ()
addWorkTree u url file key mtmp = case mtmp of
	Nothing -> go
	Just tmp -> do
		-- Move to final location for large file check.
		pruneTmpWorkDirBefore tmp $ \_ -> liftIO $ do
			createDirectoryIfMissing True (takeDirectory file)
			renameFile tmp file
		largematcher <- largeFilesMatcher
		large <- checkFileMatcher largematcher file
		if large
			then do
				-- Move back to tmp because addAnnexedFile
				-- needs the file in a different location
				-- than the work tree file.
				liftIO $ renameFile file tmp
				go
			else void $ Command.Add.addSmall file
  where
	go = do
		maybeShowJSON $ JSONChunk [("key", serializeKey key)]
		setUrlPresent key url
		logChange key u InfoPresent
		ifM (addAnnexedFile file key mtmp)
			( do
				when (isJust mtmp) $
					logStatus key InfoPresent
			, maybe noop (\tmp -> pruneTmpWorkDirBefore tmp (liftIO . nukeFile)) mtmp
			)

nodownloadWeb :: DownloadOptions -> URLString -> Url.UrlInfo -> FilePath -> Annex (Maybe Key)
nodownloadWeb o url urlinfo file
	| Url.urlExists urlinfo = if rawOption o
		then nomedia
		else either (const nomedia) usemedia
			=<< youtubeDlFileName url
	| otherwise = do
		warning $ "unable to access url: " ++ url
		return Nothing
  where
	nomedia = do
		let key = Backend.URL.fromUrl url (Url.urlSize urlinfo)
		nodownloadWeb' url key file
	usemedia mediafile = do
		let dest = if isJust (fileOption o)
			then file
			else takeFileName mediafile
		let mediaurl = setDownloader url YoutubeDownloader
		let mediakey = Backend.URL.fromUrl mediaurl Nothing
		nodownloadWeb' mediaurl mediakey dest

nodownloadWeb' :: URLString -> Key -> FilePath -> Annex (Maybe Key)
nodownloadWeb' url key file = checkCanAdd file $ do
	showDestinationFile file
	liftIO $ createDirectoryIfMissing True (parentDir file)
	addWorkTree webUUID url file key Nothing
	return (Just key)

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
		filter (not . null) $ splitc '/' fullurl

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
		warning $ file ++ " already exists; not overwriting"
		return Nothing
	, ifM ((not <$> Annex.getState Annex.force) <&&> checkIgnored file)
		( do
			warning $ "not adding " ++ file ++ " which is .gitignored (use --force to override)"
			return Nothing
		, a
		)
	)
