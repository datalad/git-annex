{- git-annex command
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.AddUrl where

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
import Annex.Perms
import Annex.UUID
import Annex.WorkTree
import Annex.YoutubeDl
import Annex.UntrustedFilePath
import Logs.Web
import Types.KeySource
import Types.UrlContents
import Annex.FileMatcher
import Logs.Location
import Utility.Metered
import Utility.HtmlDetect
import Utility.Path.Max
import Utility.Url.Parse
import qualified Utility.RawFilePath as R
import qualified Annex.Transfer as Transfer

import Network.URI
import qualified System.FilePath.ByteString as P

cmd :: Command
cmd = notBareRepo $ withAnnexOptions 
	[ jobsOption
	, jsonOptions
	, jsonProgressOption
	, backendOption
	] $
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
	, verifiableOption :: Bool
	, rawOption :: Bool
	, noRawOption :: Bool
	, rawExceptOption :: Maybe (DeferredParse Remote)
	, fileOption :: Maybe FilePath
	, preserveFilenameOption :: Bool
	, checkGitIgnoreOption :: CheckGitIgnore
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
	<*> parseBatchOption False
	<*> switch
		( long "with-files"
		<> help "parse batch mode lines of the form \"$url $file\""
		)

parseDownloadOptions :: Bool -> Parser DownloadOptions
parseDownloadOptions withfileoptions = DownloadOptions
	<$> switch
		( long "relaxed"
		<> help "accept whatever content is downloaded from web even if it changes"
		)
	<*> switch
		( long "verifiable"
		<> short 'V'
		<> help "improve later verification of --fast or --relaxed content"
		)
	<*> switch
		( long "raw"
		<> help "disable special handling for torrents, youtube-dl, etc"
		)
	<*> switch
		( long "no-raw"
		<> help "prevent downloading raw url content, must use special handling"
		)
	<*> optional
		(mkParseRemoteOption <$> strOption
		        ( long "raw-except" <> metavar paramRemote
			<> help "disable special handling except by this remote"
			<> completeRemotes
			)
		)
	<*> (if withfileoptions
		then optional (strOption
			( long "file" <> metavar paramFile
			<> help "specify what file the url is added to"
			))
		else pure Nothing)
	<*> (if withfileoptions
		then switch
			( long "preserve-filename"
			<> help "use filename provided by server as-is"
			)
		else pure False)
	<*> Command.Add.checkGitIgnoreSwitch

seek :: AddUrlOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	addunlockedmatcher <- addUnlockedMatcher
	let go (si, (o', u)) = do
		r <- checkClaimingUrl (downloadOptions o) u
		if Remote.uuid r == webUUID || rawOption (downloadOptions o')
			then void $ commandAction $
				startWeb addunlockedmatcher o' si u
			else checkUrl addunlockedmatcher r o' si u
	case batchOption o of
		Batch fmt -> batchOnly Nothing (addUrls o) $
			batchInput fmt (pure . parseBatchInput o) go
		NoBatch -> forM_ (addUrls o) (\u -> go (SeekInput [u], (o, u)))

checkClaimingUrl :: DownloadOptions -> URLString -> Annex Remote
checkClaimingUrl o u = do
	allowedremote <- case rawExceptOption o of
                Nothing -> pure (const True)
                Just f -> (==) <$> getParsed f
	Remote.claimingUrl' allowedremote u

parseBatchInput :: AddUrlOptions -> String -> Either String (AddUrlOptions, URLString)
parseBatchInput o s
	| batchFilesOption o =
		let (u, f) = separate (== ' ') s
		in if null u || null f
			then Left ("parsed empty url or filename in input: " ++ s)
			else Right (o { downloadOptions = (downloadOptions o) { fileOption = Just f } }, u)
	| otherwise = Right (o, s)

checkUrl :: AddUnlockedMatcher -> Remote -> AddUrlOptions -> SeekInput -> URLString -> Annex ()
checkUrl addunlockedmatcher r o si u = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let deffile = fromMaybe (urlString2file u (pathdepthOption o) pathmax) (fileOption (downloadOptions o))
	go deffile =<< maybe
		(giveup $ "unable to checkUrl of " ++ Remote.name r)
		(tryNonAsync . flip id u)
		(Remote.checkUrl r)
  where

	go _ (Left e) = void $ commandAction $ startingAddUrl si u o $ do
		warning (UnquotedString (show e))
		next $ return False
	go deffile (Right (UrlContents sz mf)) = do
		f <- maybe (pure deffile) (sanitizeOrPreserveFilePath o . fromOsPath) mf
		let f' = adjustFile o (fromMaybe f (fileOption (downloadOptions o)))
		void $ commandAction $ startRemote addunlockedmatcher r o si f' u sz
	go deffile (Right (UrlMulti l)) = case fileOption (downloadOptions o) of
		Nothing ->
			forM_ l $ \(u', sz, f) -> do
				f' <- sanitizeOrPreserveFilePath o (fromOsPath f)
				let f'' = adjustFile o (fromOsPath (toOsPath deffile </> toOsPath f'))
				void $ commandAction $ startRemote addunlockedmatcher r o si f'' u' sz
		Just f -> case l of
			[] -> noop
			((u',sz,_):[]) -> do
				let f' = adjustFile o f
				void $ commandAction $ startRemote addunlockedmatcher r o si f' u' sz
			_ -> giveup $ unwords
				[ "That url contains multiple files according to the"
				, Remote.name r
				, " remote; cannot add it to a single file."
				]

startRemote :: AddUnlockedMatcher -> Remote -> AddUrlOptions -> SeekInput -> FilePath -> URLString -> Maybe Integer -> CommandStart
startRemote addunlockedmatcher r o si file uri sz = do
	pathmax <- liftIO $ fileNameLengthLimit "."
	let file' = toOsPath $ P.joinPath $ map (truncateFilePath pathmax) $
		P.splitDirectories (toRawFilePath file)
	startingAddUrl si uri o $ do
		showNote $ UnquotedString $ "from " ++ Remote.name r 
		showDestinationFile file'
		performRemote addunlockedmatcher r o uri file' sz

performRemote :: AddUnlockedMatcher -> Remote -> AddUrlOptions -> URLString -> OsPath -> Maybe Integer -> CommandPerform
performRemote addunlockedmatcher r o uri file sz = lookupKey file >>= \case
	Just k -> adduri k
	Nothing -> geturi
  where
	loguri = setDownloader uri OtherDownloader
	adduri = addUrlChecked o loguri file (Remote.uuid r) checkexistssize
	checkexistssize key = return $ Just $ case sz of
		Nothing -> (True, True, loguri)
		Just n -> (True, n == fromMaybe n (fromKey keySize key), loguri)
	geturi = next $ isJust <$> downloadRemoteFile addunlockedmatcher r (downloadOptions o) uri file sz

downloadRemoteFile :: AddUnlockedMatcher -> Remote -> DownloadOptions -> URLString -> OsPath -> Maybe Integer -> Annex (Maybe Key)
downloadRemoteFile addunlockedmatcher r o uri file sz = checkCanAdd o file $ \canadd -> do
	let urlkey = Backend.URL.fromUrl uri sz (verifiableOption o)
	createWorkTreeDirectory (parentDir file)
	ifM (Annex.getRead Annex.fast <||> pure (relaxedOption o))
		( do
			addWorkTree canadd addunlockedmatcher (Remote.uuid r) loguri file urlkey Nothing
			return (Just urlkey)
		, do
			-- Set temporary url for the urlkey
			-- so that the remote knows what url it
			-- should use to download it.
			setTempUrl urlkey loguri
			let downloader = \dest p ->
				fst <$> Remote.verifiedAction
					(Remote.retrieveKeyFile r urlkey af dest p (RemoteVerify r))
			ret <- downloadWith canadd addunlockedmatcher downloader urlkey (Remote.uuid r) loguri file
			removeTempUrl urlkey
			return ret
		)
  where
	loguri = setDownloader uri OtherDownloader
	af = AssociatedFile (Just file)

startWeb :: AddUnlockedMatcher -> AddUrlOptions -> SeekInput -> URLString -> CommandStart
startWeb addunlockedmatcher o si urlstring = go $ fromMaybe bad $ parseURIPortable urlstring
  where
	bad = fromMaybe (giveup $ "bad url " ++ urlstring) $
		Url.parseURIRelaxed $ urlstring
	go url = startingAddUrl si urlstring o $
		if relaxedOption (downloadOptions o)
			then go' url Url.assumeUrlExists
			else Url.withUrlOptions (Url.getUrlInfo urlstring) >>= \case
				Right urlinfo -> go' url urlinfo
				Left err -> do
					warning (UnquotedString err)
					next $ return False
	go' url urlinfo = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		file <- adjustFile o <$> case fileOption (downloadOptions o) of
			Just f -> pure f
			Nothing -> case Url.urlSuggestedFile urlinfo of
				Just sf -> do
					f <- sanitizeOrPreserveFilePath o sf
					if preserveFilenameOption (downloadOptions o)
						then pure f
						else ifM (liftIO $ doesFileExist (toOsPath f) <||> doesDirectoryExist (toOsPath f))
							( pure $ url2file url (pathdepthOption o) pathmax
							, pure f
							)
				_ -> pure $ url2file url (pathdepthOption o) pathmax
		performWeb addunlockedmatcher o urlstring (toOsPath file) urlinfo

sanitizeOrPreserveFilePath :: AddUrlOptions -> FilePath -> Annex FilePath
sanitizeOrPreserveFilePath o f
	| preserveFilenameOption (downloadOptions o) && not (null f) = do
		checkPreserveFileNameSecurity f
		return f
	| otherwise = do
		pathmax <- liftIO $ fileNameLengthLimit "."
		return $ fromRawFilePath $ truncateFilePath pathmax $
			toRawFilePath $ sanitizeFilePath f

-- sanitizeFilePath avoids all these security problems
-- (and probably others, but at least this catches the most egrarious ones).
checkPreserveFileNameSecurity :: FilePath -> Annex ()
checkPreserveFileNameSecurity f = do
	checksecurity controlCharacterInFilePath "control character"
	checksecurity pathTraversalInFilePath "path traversal"
	checksecurity gitDirectoryInFilePath "contains a .git directory"
  where
	checksecurity p d = when (p f) $ do
		qp <- coreQuotePath <$> Annex.getGitConfig
		giveup $ decodeBS $ quote qp $
			"--preserve-filename was used, but the filename ("
				<> QuotedPath (toOsPath f)
				<> ") has a security problem ("
				<> d
				<> "), not adding."

performWeb :: AddUnlockedMatcher -> AddUrlOptions -> URLString -> OsPath -> Url.UrlInfo -> CommandPerform
performWeb addunlockedmatcher o url file urlinfo = lookupKey file >>= \case
	Just k -> addurl k
	Nothing -> geturl
  where
	geturl = next $ isJust <$> addUrlFile addunlockedmatcher (downloadOptions o) url urlinfo file
	addurl = addUrlChecked o url file webUUID $ \k ->
		ifM (useYoutubeDl (downloadOptions o) <&&> youtubeDlSupported url)
			( return (Just (True, True, setDownloader url YoutubeDownloader))
			, checkRaw Nothing (downloadOptions o) (pure Nothing) $
				return (Just (Url.urlExists urlinfo, Url.urlSize urlinfo == fromKey keySize k, url))
			)

{- Check that the url exists, and has the same size as the key,
 - and add it as an url to the key. -}
addUrlChecked :: AddUrlOptions -> URLString -> OsPath -> UUID -> (Key -> Annex (Maybe (Bool, Bool, URLString))) -> Key -> CommandPerform
addUrlChecked o url file u checkexistssize key =
	ifM ((elem url <$> getUrls key) <&&> (elem u <$> loggedLocations key))
		( do
			showDestinationFile file
			next $ return True
		, checkexistssize key >>= \case
			Just (exists, samesize, url')
				| exists && (samesize || relaxedOption (downloadOptions o)) -> do
					setUrlPresent key url'
					logChange NoLiveUpdate key u InfoPresent
					next $ return True
				| otherwise -> do
					warning $ UnquotedString $ "while adding a new url to an already annexed file, " ++ if exists
						then "url does not have expected file size (use --relaxed to bypass this check) " ++ url
						else "failed to verify url exists: " ++ url
					stop
			Nothing -> stop
		)

{- Downloads an url (except in fast or relaxed mode) and adds it to the
 - repository, normally at the specified FilePath. 
 - But, if youtube-dl supports the url, it will be written to a
 - different file, based on the title of the media. Unless the user
 - specified fileOption, which then forces using the FilePath.
 -}
addUrlFile :: AddUnlockedMatcher -> DownloadOptions -> URLString -> Url.UrlInfo -> OsPath -> Annex (Maybe Key)
addUrlFile addunlockedmatcher o url urlinfo file =
	ifM (Annex.getRead Annex.fast <||> pure (relaxedOption o))
		( nodownloadWeb addunlockedmatcher o url urlinfo file
		, downloadWeb addunlockedmatcher o url urlinfo file
		)

downloadWeb :: AddUnlockedMatcher -> DownloadOptions -> URLString -> Url.UrlInfo -> OsPath -> Annex (Maybe Key)
downloadWeb addunlockedmatcher o url urlinfo file =
	go =<< downloadWith' downloader urlkey webUUID url file
  where
	urlkey = addSizeUrlKey urlinfo $ Backend.URL.fromUrl url Nothing (verifiableOption o)
	downloader f p = Url.withUrlOptions $ downloadUrl False urlkey p Nothing [url] f
	go Nothing = return Nothing
	go (Just (tmp, backend)) = ifM (useYoutubeDl o <&&> liftIO (isHtmlFile tmp))
		( tryyoutubedl tmp backend
		, normalfinish tmp backend
		)
	normalfinish tmp backend = checkCanAdd o file $ \canadd -> do
		showDestinationFile file
		createWorkTreeDirectory (parentDir file)
		Just <$> finishDownloadWith canadd addunlockedmatcher tmp backend webUUID url file
	-- Ask youtube-dl what filename it will download first, 
	-- so it's only used when the file contains embedded media.
	tryyoutubedl tmp backend = youtubeDlFileNameHtmlOnly url >>= \case
		Right mediafile -> do
			liftIO $ liftIO $ removeWhenExistsWith removeFile tmp
			let f = youtubeDlDestFile o file mediafile
			lookupKey f >>= \case
				Just k -> alreadyannexed f k
				Nothing -> dl f
		Left err -> checkRaw (Just err) o (pure Nothing) (normalfinish tmp backend)
	  where
		dl dest = withTmpWorkDir mediakey $ \workdir -> do
			let cleanuptmp = pruneTmpWorkDirBefore tmp (liftIO . removeWhenExistsWith removeFile)
			dlcmd <- youtubeDlCommand
			showNote ("using " <> UnquotedString dlcmd)
			Transfer.notifyTransfer Transfer.Download url $
				Transfer.download' webUUID mediakey (AssociatedFile Nothing) Nothing Transfer.noRetry $ \p -> do
					showDestinationFile dest
					youtubeDl url workdir p >>= \case
						Right (Just mediafile) -> do
							cleanuptmp
							checkCanAdd o dest $ \canadd -> do
								addWorkTree canadd addunlockedmatcher webUUID mediaurl dest mediakey (Just mediafile)
								return $ Just mediakey
						Left msg -> do
							cleanuptmp
							warning (UnquotedString msg)
							return Nothing
						Right Nothing -> do
							cleanuptmp
							warning (UnquotedString dlcmd <> " did not download anything")
							return Nothing
		mediaurl = setDownloader url YoutubeDownloader
		mediakey = Backend.URL.fromUrl mediaurl Nothing (verifiableOption o)
		-- Does the already annexed file have the mediaurl
		-- as an url? If so nothing to do.
		alreadyannexed dest k = do
			us <- getUrls k
			if mediaurl `elem` us
				then return (Just k)
				else do
					warning $ QuotedPath dest <> " already exists; not overwriting"
					return Nothing
	
checkRaw :: (Maybe String) -> DownloadOptions -> Annex a -> Annex a -> Annex a
checkRaw failreason o f a
	| noRawOption o = do
		warning $ UnquotedString $ "Unable to use youtube-dl or a special remote and --no-raw was specified" ++
			case failreason of
				Just msg -> ": " ++ msg
				Nothing -> ""
		f
	| otherwise = a

useYoutubeDl :: DownloadOptions -> Annex Bool
useYoutubeDl o
	| rawOption o = pure False
	| otherwise = case rawExceptOption o of
                Nothing -> pure True
                Just f -> do
			remote <- getParsed f
			pure (Remote.uuid remote == webUUID)

{- The destination file is not known at start time unless the user provided
 - a filename. It's not displayed then for output consistency, 
 - but is added to the json when available. -}
startingAddUrl :: SeekInput -> URLString -> AddUrlOptions -> CommandPerform -> CommandStart
startingAddUrl si url o p = starting "addurl" ai si $ do
	case fileOption (downloadOptions o) of
		Nothing -> noop
		Just file -> maybeShowJSON $ JSONChunk [("file", file)]
	p
  where
	-- Avoid failure when the same url is downloaded concurrently
	-- to two different files, by using OnlyActionOn with a key
	-- based on the url. Note that this may not be the actual key
	-- that is used for the download; later size information may be
	-- available and get added to it. That's ok, this is only
	-- used to prevent two threads running concurrently when that would
	-- likely fail.
	ai = OnlyActionOn urlkey (ActionItemOther (Just (UnquotedString url)))
	urlkey = Backend.URL.fromUrl url Nothing (verifiableOption (downloadOptions o))

showDestinationFile :: OsPath -> Annex ()
showDestinationFile file = do
	showNote ("to " <> QuotedPath file)
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
downloadWith :: CanAddFile -> AddUnlockedMatcher -> (OsPath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> OsPath -> Annex (Maybe Key)
downloadWith canadd addunlockedmatcher downloader dummykey u url file =
	go =<< downloadWith' downloader dummykey u url file
  where
	go Nothing = return Nothing
	go (Just (tmp, backend)) = Just <$> finishDownloadWith canadd addunlockedmatcher tmp backend u url file

{- Like downloadWith, but leaves the dummy key content in
 - the returned location. -}
downloadWith' :: (OsPath -> MeterUpdate -> Annex Bool) -> Key -> UUID -> URLString -> OsPath -> Annex (Maybe (OsPath, Backend))
downloadWith' downloader dummykey u url file =
	checkDiskSpaceToGet dummykey Nothing Nothing $ do
		backend <- chooseBackend file
		tmp <- fromRepo $ gitAnnexTmpObjectLocation dummykey
		let t = (Transfer.Transfer Transfer.Download u (fromKey id dummykey))
		ok <- Transfer.notifyTransfer Transfer.Download url $ \_w ->
			Transfer.runTransfer t (Just backend) afile Nothing Transfer.stdRetry $ \p -> do
				createAnnexDirectory (parentDir tmp)
				downloader tmp p
		if ok
			then return (Just (tmp, backend))
			else return Nothing
  where
	afile = AssociatedFile (Just file)

finishDownloadWith :: CanAddFile -> AddUnlockedMatcher -> OsPath -> Backend -> UUID -> URLString -> OsPath -> Annex Key
finishDownloadWith canadd addunlockedmatcher tmp backend u url file = do
	let source = KeySource
		{ keyFilename = file
		, contentLocation = tmp
		, inodeCache = Nothing
		}
	key <- fst <$> genKey source nullMeterUpdate backend
	addWorkTree canadd addunlockedmatcher u url file key (Just tmp)
	return key

{- Adds the url size to the Key. -}
addSizeUrlKey :: Url.UrlInfo -> Key -> Key
addSizeUrlKey urlinfo key = alterKey key $ \d -> d
	{ keySize = Url.urlSize urlinfo
	}

{- Adds worktree file to the repository. -}
addWorkTree :: CanAddFile -> AddUnlockedMatcher -> UUID -> URLString -> OsPath -> Key -> Maybe OsPath -> Annex ()
addWorkTree _ addunlockedmatcher u url file key mtmp = case mtmp of
	Nothing -> go
	Just tmp -> do
		s <- liftIO $ R.getSymbolicLinkStatus (fromOsPath tmp)
		-- Move to final location for large file check.
		pruneTmpWorkDirBefore tmp $ \_ -> do
			createWorkTreeDirectory (takeDirectory file)
			liftIO $ moveFile tmp file
		largematcher <- largeFilesMatcher
		large <- checkFileMatcher NoLiveUpdate largematcher file
		if large
			then do
				-- Move back to tmp because addAnnexedFile
				-- needs the file in a different location
				-- than the work tree file.
				liftIO $ moveFile file tmp
				go
			else Command.Add.addSmall False (DryRun False) file s
				>>= maybe noop void
  where
	go = do
		maybeShowJSON $ JSONChunk [("key", serializeKey key)]
		setUrlPresent key url
		logChange NoLiveUpdate key u InfoPresent
		ifM (addAnnexedFile addunlockedmatcher file key mtmp)
			( do
				when (isJust mtmp) $
					logStatus NoLiveUpdate key InfoPresent
			, maybe noop (\tmp -> pruneTmpWorkDirBefore tmp (liftIO . removeWhenExistsWith removeFile)) mtmp
			)

nodownloadWeb :: AddUnlockedMatcher -> DownloadOptions -> URLString -> Url.UrlInfo -> OsPath -> Annex (Maybe Key)
nodownloadWeb addunlockedmatcher o url urlinfo file
	| Url.urlExists urlinfo = if rawOption o
		then nomedia
		else youtubeDlFileName url >>= \case
			Right mediafile -> usemedia mediafile
			Left err -> checkRaw (Just err) o (pure Nothing) nomedia
	| otherwise = do
		warning $ UnquotedString $ "unable to access url: " ++ url
		return Nothing
  where
	nomedia = do
		let key = Backend.URL.fromUrl url (Url.urlSize urlinfo) (verifiableOption o)
		nodownloadWeb' o addunlockedmatcher url key file
	usemedia mediafile = do
		let dest = youtubeDlDestFile o file mediafile
		let mediaurl = setDownloader url YoutubeDownloader
		let mediakey = Backend.URL.fromUrl mediaurl Nothing (verifiableOption o)
		nodownloadWeb' o addunlockedmatcher mediaurl mediakey dest

youtubeDlDestFile :: DownloadOptions -> OsPath -> OsPath -> OsPath
youtubeDlDestFile o destfile mediafile
	| isJust (fileOption o) = destfile
	| otherwise = takeFileName mediafile

nodownloadWeb' :: DownloadOptions -> AddUnlockedMatcher -> URLString -> Key -> OsPath -> Annex (Maybe Key)
nodownloadWeb' o addunlockedmatcher url key file = checkCanAdd o file $ \canadd -> do
	showDestinationFile file
	createWorkTreeDirectory (parentDir file)
	addWorkTree canadd addunlockedmatcher webUUID url file key Nothing
	return (Just key)

url2file :: URI -> Maybe Int -> Int -> FilePath
url2file url pathdepth pathmax = case pathdepth of
	Nothing -> truncatesanitize fullurl
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
	urlbits = map truncatesanitize $
		filter (not . null) $ splitc '/' fullurl
	truncatesanitize = fromRawFilePath 
		. truncateFilePath pathmax 
		. toRawFilePath 
		. sanitizeFilePath

urlString2file :: URLString -> Maybe Int -> Int -> FilePath
urlString2file s pathdepth pathmax = case Url.parseURIRelaxed s of
	Nothing -> giveup $ "bad uri " ++ s
	Just u -> url2file u pathdepth pathmax

adjustFile :: AddUrlOptions -> FilePath -> FilePath
adjustFile o = addprefix . addsuffix
  where
	addprefix f = maybe f (++ f) (prefixOption o)
	addsuffix f = maybe f (f ++) (suffixOption o)

data CanAddFile = CanAddFile

checkCanAdd :: DownloadOptions -> OsPath -> (CanAddFile -> Annex (Maybe a)) -> Annex (Maybe a)
checkCanAdd o file a = ifM (isJust <$> (liftIO $ catchMaybeIO $ R.getSymbolicLinkStatus (fromOsPath file)))
	( do
		warning $ QuotedPath file <> " already exists; not overwriting"
		return Nothing
	, ifM (checkIgnored (checkGitIgnoreOption o) file)
		( do
			warning $ "not adding " <> QuotedPath file <> " which is .gitignored (use --no-check-gitignore to override)"
			return Nothing
		, a CanAddFile
		)
	)
