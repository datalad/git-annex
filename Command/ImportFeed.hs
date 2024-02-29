{- git-annex command
 -
 - Copyright 2013-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Command.ImportFeed where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.FilePath.ByteString as P
import qualified Data.ByteString as B

import Command
import qualified Annex
import qualified Annex.Url as Url
import qualified Remote
import qualified Types.Remote as Remote
import Types.UrlContents
import Logs.Web
import Logs.File
import qualified Utility.Format
import Utility.Tmp
import Utility.Metered
import Command.AddUrl (addUrlFile, downloadRemoteFile, parseDownloadOptions, DownloadOptions(..), checkClaimingUrl, checkCanAdd, addWorkTree, checkRaw, useYoutubeDl)
import Annex.UUID
import Backend.URL (fromUrl)
import Annex.Content
import Annex.WorkTree
import Annex.YoutubeDl
import Types.MetaData
import Logs.MetaData
import Annex.MetaData
import Annex.FileMatcher
import Annex.UntrustedFilePath
import qualified Utility.RawFilePath as R
import qualified Database.ImportFeed as Db

cmd :: Command
cmd = notBareRepo $ withAnnexOptions os $
	command "importfeed" SectionCommon "import files from podcast feeds"
		(paramRepeating paramUrl) (seek <$$> optParser)
  where
	os = [jobsOption, jsonOptions, jsonProgressOption, backendOption]

data ImportFeedOptions = ImportFeedOptions
	{ feedUrls :: CmdParams
	, templateOption :: Maybe String
	, scrapeOption :: Bool
	, downloadOptions :: DownloadOptions
	}

optParser :: CmdParamsDesc -> Parser ImportFeedOptions
optParser desc = ImportFeedOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "template" <> metavar paramFormat
		<> help "template for filenames"
		))
	<*> switch
		( long "scrape"
		<> help "scrape website for content to import"
		)
	<*> parseDownloadOptions False

seek :: ImportFeedOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	addunlockedmatcher <- addUnlockedMatcher
	cache <- getCache (templateOption o)
	dlst <- liftIO $ newTMVarIO M.empty
	checkst <- liftIO $ newTVarIO M.empty

	forM_ (feedUrls o) $ \url -> do
		liftIO $ atomically $ do
			m <- takeTMVar dlst
			putTMVar dlst (M.insert url Nothing m)
		commandAction $ getFeed o url dlst
		startpendingdownloads addunlockedmatcher cache dlst checkst False
	
	startpendingdownloads addunlockedmatcher cache dlst checkst True

	clearfeedproblems checkst
  where
  	getpendingdownloads dlst blocking
		| blocking = do
			m <- takeTMVar dlst
			if M.null m
				then do
					putTMVar dlst m
					return m
				else
					let (pending, rest) = M.partition ispending m
					in if M.null pending || not (M.null rest)
						then retry
						else do
							putTMVar dlst rest
							return pending
		| otherwise = do
			m <- takeTMVar dlst
			let (pending, rest) = M.partition ispending m
			putTMVar dlst rest
			return pending
	  where
		ispending Nothing = False
		ispending (Just _) = True

	startpendingdownloads addunlockedmatcher cache dlst checkst blocking = do
		m <- liftIO $ atomically $ getpendingdownloads dlst blocking
		
		forM_ (M.toList m) $ \(url, v) -> case v of
			Nothing -> noop
			Just Nothing -> noop
			Just (Just is) -> 
				forM_ is $ \i -> do
					cv <- liftIO newEmptyTMVarIO
					liftIO $ atomically $ modifyTVar checkst $
						M.insertWith (++) url [cv]
					commandAction $
						startDownload addunlockedmatcher o cache cv i
	
	clearfeedproblems checkst = do
		m <- liftIO $ atomically $ readTVar checkst
		forM_ (M.toList m) $ \(url, cvl) ->
			whenM (and <$> mapM (liftIO . atomically . takeTMVar) cvl) $
				clearFeedProblem url

getFeed
	:: ImportFeedOptions
	-> URLString
	-> TMVar (M.Map URLString (Maybe (Maybe [ToDownload])))
	-> CommandStart
getFeed o url st =
	starting "importfeed" (ActionItemOther (Just (UnquotedString url))) (SeekInput [url]) $
		go `onException` recordfail
  where
	record v = liftIO $ atomically $ do
		m <- takeTMVar st
		putTMVar st (M.insert url v m)
	recordfail = record (Just Nothing)
	
	go
		| scrapeOption o = scrape
		| otherwise = get

	get = withTmpFile "feed" $ \tmpf h -> do
		liftIO $ hClose h
		ifM (downloadFeed url tmpf)
			( parse tmpf
			, do
				recordfail
				next $ feedProblem url
					"downloading the feed failed"
			)

	-- Use parseFeedFromFile rather than reading the file
	-- ourselves because it goes out of its way to handle encodings.
	parse tmpf = liftIO (parseFeedFromFile' tmpf) >>= \case
		Nothing -> debugfeedcontent tmpf "parsing the feed failed"
		Just f -> do
			case decodeBS $ fromFeedText $ getFeedTitle f of
				"" -> noop
				t -> showNote (UnquotedString ('"' : t ++ "\""))
			case findDownloads url f of
				[] -> debugfeedcontent tmpf "bad feed content; no enclosures to download"
				l -> do
					record (Just (Just l))
					next $ return True
	
	debugfeedcontent tmpf msg = do
		feedcontent <- liftIO $ readFile tmpf
		fastDebug "Command.ImportFeed" $ unlines
			[ "start of feed content"
			, feedcontent
			, "end of feed content"
			]
		recordfail
		next $ feedProblem url
			(msg ++ " (use --debug --debugfilter=ImportFeed to see the feed content that was downloaded)")
		
	scrape = youtubePlaylist url >>= \case
		Left err -> do
			recordfail
			next $ feedProblem url err
		Right playlist -> do
			record (Just (Just (playlistDownloads url playlist)))
			next $ return True

parseFeedFromFile' :: FilePath -> IO (Maybe Feed)
#if MIN_VERSION_feed(1,1,0)
parseFeedFromFile' = parseFeedFromFile
#else
parseFeedFromFile' f = catchMaybeIO (parseFeedFromFile f)
#endif

data ToDownload = ToDownload
	{ feedurl :: URLString
	, location :: DownloadLocation
	, itemid :: Maybe B.ByteString
	-- Either the parsed or unparsed date.
	, itempubdate :: Maybe (Either String UTCTime)
	-- Fields that are used as metadata and to generate the filename.
	, itemfields :: [(String, String)]
	-- True when youtube-dl found this by scraping, so certainly
	-- supports downloading it.
	, youtubedlscraped :: Bool
	}

data DownloadLocation = Enclosure URLString | MediaLink URLString

type ItemId = String

data Cache = Cache
	{ dbhandle :: Maybe Db.ImportFeedDbHandle 
	, template :: Utility.Format.Format
	}

getCache :: Maybe String -> Annex Cache
getCache opttemplate = ifM (Annex.getRead Annex.force)
	( ret Nothing
	, do
		j <- jsonOutputEnabled
		unless j $
			showStartMessage (StartMessage "importfeed" (ActionItemOther (Just "gathering known urls")) (SeekInput []))
		h <- Db.openDb
		unless j
			showEndOk
		ret (Just h)
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret h = return $ Cache h tmpl

findDownloads :: URLString -> Feed -> [ToDownload]
findDownloads u f = catMaybes $ map mk (feedItems f)
  where
	mk i = case getItemEnclosure i of
		Just (enclosureurl, _, _) ->
			Just $ mk' i
				(Enclosure $ decodeBS $ fromFeedText enclosureurl)
		Nothing -> case getItemLink i of
			Just l -> Just $ mk' i
				(MediaLink $ decodeBS $ fromFeedText l)
			Nothing -> Nothing
	mk' i l = ToDownload
		{ feedurl = u
		, location = l
		, itemid = case getItemId i of
			Just (_, iid) -> Just (fromFeedText iid)
			_ -> Nothing
		, itempubdate = case getItemPublishDate i :: Maybe (Maybe UTCTime) of
			Just (Just d) -> Just (Right d)
			_ -> Left . decodeBS . fromFeedText 
				<$> getItemPublishDateString  i
		, itemfields = extractFeedItemFields f i u
		, youtubedlscraped = False
		}

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> FilePath -> Annex Bool
downloadFeed url f
	| Url.parseURIRelaxed url == Nothing = giveup "invalid feed url"
	| otherwise = Url.withUrlOptions $
		Url.download nullMeterUpdate Nothing url f

startDownload :: AddUnlockedMatcher -> ImportFeedOptions -> Cache -> TMVar Bool -> ToDownload -> CommandStart
startDownload addunlockedmatcher opts cache cv todownload = case location todownload of
	Enclosure url -> startdownloadenclosure url
	MediaLink linkurl -> do
		let mediaurl = setDownloader linkurl YoutubeDownloader
		let mediakey = Backend.URL.fromUrl mediaurl Nothing (verifiableOption (downloadOptions opts))
		-- Old versions of git-annex that used quvi might have
		-- used the quviurl for this, so check if it's known
		-- to avoid adding it a second time.
		let quviurl = setDownloader linkurl QuviDownloader
		checkknown mediaurl $ checkknown quviurl $
			ifM (Annex.getRead Annex.fast <||> pure (relaxedOption (downloadOptions opts)))
				( addmediafast linkurl mediaurl mediakey
				, downloadmedia linkurl mediaurl mediakey
				)
  where
	{- Avoids downloading any items that are already known to be
	 - associated with a file in the annex. -}
	checkknown url a = case dbhandle cache of
		Just db -> ifM (liftIO $ Db.isKnownUrl db url)
			( nothingtodo
			, case itemid todownload of
				Just iid ->
					ifM (liftIO $ Db.isKnownItemId db iid)
						( nothingtodo
						, a
						)
				Nothing -> a
			)
		Nothing -> a

	nothingtodo = recordsuccess >> stop

	recordsuccess = liftIO $ atomically $ putTMVar cv True
	
	startdownloadenclosure :: URLString -> CommandStart
	startdownloadenclosure url = checkknown url $ startUrlDownload cv todownload url $
		downloadEnclosure addunlockedmatcher opts cache cv todownload url 

	downloadmedia linkurl mediaurl mediakey =
		ifM (useYoutubeDl (downloadOptions opts) <&&> youtubeDlSupported linkurl)
			( startUrlDownload cv todownload linkurl $
				withTmpWorkDir mediakey $ \workdir -> do
					dl <- youtubeDl linkurl (fromRawFilePath workdir) nullMeterUpdate
					case dl of
						Right (Just mediafile) -> do
							let ext = case takeExtension mediafile of
								[] -> ".m"
								s -> s
							runDownload todownload linkurl ext cache cv $ \f ->
								checkCanAdd (downloadOptions opts) f $ \canadd -> do
									addWorkTree canadd addunlockedmatcher webUUID mediaurl f mediakey (Just (toRawFilePath mediafile))
									return (Just [mediakey])
						-- youtube-dl didn't support it, so
						-- download it as if the link were
						-- an enclosure.
						Right Nothing -> contdownloadlink
						Left msg -> do
							warning $ UnquotedString $ linkurl ++ ": " ++ msg
							liftIO $ atomically $ putTMVar cv False
							next $ return False
			, startdownloadlink
			)
	  where
		startdownloadlink = checkRaw (Just linkurl) (downloadOptions opts) nothingtodo $
			startdownloadenclosure linkurl
		contdownloadlink = downloadEnclosure addunlockedmatcher opts cache cv todownload linkurl

	addmediafast linkurl mediaurl mediakey =
		ifM (useYoutubeDl (downloadOptions opts)
		     <&&> (pure (youtubedlscraped todownload) <||> youtubeDlSupported linkurl))
			( startUrlDownload cv todownload linkurl $ do
				runDownload todownload linkurl ".m" cache cv $ \f ->
					checkCanAdd (downloadOptions opts) f $ \canadd -> do
						addWorkTree canadd addunlockedmatcher webUUID mediaurl f mediakey Nothing
						return (Just [mediakey])
			, startdownloadenclosure linkurl
			)

downloadEnclosure :: AddUnlockedMatcher -> ImportFeedOptions -> Cache -> TMVar Bool -> ToDownload -> URLString -> CommandPerform
downloadEnclosure addunlockedmatcher opts cache cv todownload url = 
	runDownload todownload url (takeWhile (/= '?') $ takeExtension url) cache cv $ \f -> do
		let f' = fromRawFilePath f
		r <- checkClaimingUrl (downloadOptions opts) url
		if Remote.uuid r == webUUID || rawOption (downloadOptions opts)
			then checkRaw (Just url) (downloadOptions opts) (pure Nothing) $ do
				let dlopts = (downloadOptions opts)
					-- force using the filename
					-- chosen here
					{ fileOption = Just f'
					-- don't use youtube-dl
					, rawOption = True
					}
				let go urlinfo = Just . maybeToList <$> addUrlFile addunlockedmatcher dlopts url urlinfo f
				if relaxedOption (downloadOptions opts)
					then go Url.assumeUrlExists
					else Url.withUrlOptions (Url.getUrlInfo url) >>= \case
						Right urlinfo -> go urlinfo
						Left err -> do
							warning (UnquotedString err)
							return (Just [])
			else do
				res <- tryNonAsync $ maybe
					(giveup $ "unable to checkUrl of " ++ Remote.name r)
					(flip id url)
					(Remote.checkUrl r)
				case res of
					Left _ -> return (Just [])
					Right (UrlContents sz _) ->
						Just . maybeToList <$>
							downloadRemoteFile addunlockedmatcher r (downloadOptions opts) url f sz
					Right (UrlMulti l) -> do
						kl <- forM l $ \(url', sz, subf) ->
							let dest = f P.</> toRawFilePath (sanitizeFilePath subf)
							in downloadRemoteFile addunlockedmatcher r (downloadOptions opts) url' dest sz
						return $ Just $ if all isJust kl
							then catMaybes kl
							else []

runDownload
	:: ToDownload
	-> URLString
	-> String
	-> Cache
	-> TMVar Bool
	-> (RawFilePath -> Annex (Maybe [Key]))
	-> CommandPerform
runDownload todownload url extension cache cv getter = do
	dest <- makeunique (1 :: Integer) $
		feedFile (template cache) todownload extension
	case dest of
		Nothing -> do
			recordsuccess
			next $ return True
		Just f -> getter (toRawFilePath f) >>= \case
			Just ks
				-- Download problem.
				| null ks -> do
					broken <- checkFeedBroken (feedurl todownload)
					when broken $
						void $ feedProblem url "download failed"
					liftIO $ atomically $ putTMVar cv broken
					next $ return False
				| otherwise -> do
					forM_ ks $ \key ->
						ifM (annexGenMetaData <$> Annex.getGitConfig)
							( addMetaData key $ extractMetaData todownload
							, addMetaData key $ minimalMetaData todownload
							)
					recordsuccess
					next $  return True
			-- Was not able to add anything, but not
			-- because of a download problem.
			Nothing -> do
				recordsuccess
				next $ return False
  where
	recordsuccess = liftIO $ atomically $ putTMVar cv True

	forced = Annex.getRead Annex.force

	{- Find a unique filename to save the url to.
	 - If the file exists, prefixes it with a number.
	 - When forced, the file may already exist and have the same
	 - url, in which case Nothing is returned as it does not need
	 - to be re-downloaded. -}
	makeunique n file = ifM alreadyexists
		( ifM forced
			( lookupKey (toRawFilePath f) >>= \case
				Just k -> checksameurl k
				Nothing -> tryanother
			, tryanother
			)
		, return $ Just f
		)
	  where
		f = if n < 2
			then file
			else
				let (d, base) = splitFileName file
				in d </> show n ++ "_" ++ base
		tryanother = makeunique (n + 1) file
		alreadyexists = liftIO $ isJust <$> catchMaybeIO (R.getSymbolicLinkStatus (toRawFilePath f))
		checksameurl k = ifM (elem url . map fst . map getDownloader <$> getUrls k)
			( return Nothing
			, tryanother
			)

startUrlDownload :: TMVar Bool -> ToDownload -> URLString -> CommandPerform -> CommandStart
startUrlDownload cv todownload url a = do
	starting "addurl"
		(ActionItemOther (Just (UnquotedString url)))
		(SeekInput [feedurl todownload])
		(go `onException` recordfailure)
  where
	recordfailure = do
		void $ feedProblem url "download failed"
		liftIO $ atomically $ tryPutTMVar cv False
	go = do
		maybeAddJSONField "url" url
		a

defaultTemplate :: String
defaultTemplate = "${feedtitle}/${itemtitle}${extension}"

{- Generates a filename to use for a feed item by filling out the template.
 - The filename may not be unique. -}
feedFile :: Utility.Format.Format -> ToDownload -> String -> FilePath
feedFile tmpl i extension = sanitizeLeadingFilePathCharacter $ 
	Utility.Format.format tmpl $
		M.map sanitizeFilePathComponent $ M.fromList $ itemfields i ++
			[ ("extension", extension)
			, extractField "itempubdate" [itempubdatestring]
			, extractField "itempubyear" [itempubyear]
			, extractField "itempubmonth" [itempubmonth]
			, extractField "itempubday" [itempubday]
			, extractField "itempubhour" [itempubhour]
			, extractField "itempubminute" [itempubminute]
			, extractField "itempubsecond" [itempubsecond]
			]
  where
	pubdate = maybe Nothing eitherToMaybe (itempubdate i)
	
	itempubdatestring = case itempubdate i of
		Just (Right pd) -> Just $ formatTime defaultTimeLocale "%F" pd
		-- if date cannot be parsed, use the raw string
		Just (Left s) -> Just $ replace "/" "-" s
		Nothing -> Nothing
	
	(itempubyear, itempubmonth, itempubday) = case pubdate of
		Nothing -> (Nothing, Nothing, Nothing)
		Just pd -> 
			let (y, m, d) = toGregorian (utctDay pd)
			in (Just (show y), Just (show m), Just (show d))
	
	(itempubhour, itempubminute, itempubsecond) = case pubdate of
		Nothing -> (Nothing, Nothing, Nothing)
		Just pd -> 
			let tod = timeToTimeOfDay (utctDayTime pd)
			in ( Just (show (todHour tod))
			   , Just (show (todMin tod))
			   -- avoid fractional seconds
			   , Just (takeWhile (/= '.') (show (todSec tod)))
			   )

extractMetaData :: ToDownload -> MetaData
extractMetaData i = case itempubdate i of
	Just (Right d) -> unionMetaData meta (dateMetaData d meta)
	_ -> meta
  where
	tometa (k, v) = (mkMetaFieldUnchecked (T.pack k), S.singleton (toMetaValue (encodeBS v)))
	meta = MetaData $ M.fromList $ map tometa $ itemfields i

minimalMetaData :: ToDownload -> MetaData
minimalMetaData i = case itemid i of
	Nothing -> emptyMetaData
	Just iid -> MetaData $ M.singleton itemIdField
		(S.singleton $ toMetaValue iid)

noneValue :: String
noneValue = "none"

extractField :: String -> [Maybe String] -> (String, String)
extractField k [] = (k, noneValue)
extractField k (Just v:_)
	| not (null v) = (k, v)
extractField k (_:rest) = extractField k rest

extractFeedItemFields :: Feed -> Item -> URLString -> [(String, String)]
extractFeedItemFields f i u = map (uncurry extractField)
	[ ("feedurl", [Just u])
	, ("feedtitle", [feedtitle])
	, ("itemtitle", [itemtitle])
	, ("feedauthor", [feedauthor])
	, ("itemauthor", [itemauthor])
	, ("itemsummary", [decodeBS . fromFeedText <$> getItemSummary i])
	, ("itemdescription", [decodeBS . fromFeedText <$> getItemDescription i])
	, ("itemrights", [decodeBS . fromFeedText <$> getItemRights i])
	, ("itemid", [decodeBS . fromFeedText . snd <$> getItemId i])
	, ("title", [itemtitle, feedtitle])
	, ("author", [itemauthor, feedauthor])
	]
  where
	feedtitle = Just $ decodeBS $ fromFeedText $ getFeedTitle f
	itemtitle = decodeBS . fromFeedText <$> getItemTitle i
	feedauthor = decodeBS . fromFeedText <$> getFeedAuthor f
	itemauthor = decodeBS . fromFeedText <$> getItemAuthor i

playlistFields :: URLString -> YoutubePlaylistItem -> [(String, String)]
playlistFields u i = map (uncurry extractField)
	[ ("feedurl", [Just u])
	, ("feedtitle", [youtube_playlist_title i])
	, ("itemtitle", [youtube_title i])
	, ("feedauthor", [youtube_playlist_uploader i])
	, ("itemauthor", [youtube_playlist_uploader i])
	-- itemsummary omitted, no equivilant in yt-dlp data
	, ("itemdescription", [youtube_description i])
	, ("itemrights", [youtube_license i])
	, ("itemid", [youtube_url i])
	, ("title", [youtube_title i, youtube_playlist_title i])
	, ("author", [youtube_playlist_uploader i])
	]

playlistDownloads :: URLString -> [YoutubePlaylistItem] -> [ToDownload]
playlistDownloads url = mapMaybe go
  where
	go i = do
		iurl <- youtube_url i
		return $ ToDownload
			{ feedurl = url
			, location = MediaLink iurl
			, itemid = Just (encodeBS iurl)
			, itempubdate = 
				Right . posixSecondsToUTCTime . fromIntegral
					<$> youtube_timestamp i
			, itemfields = playlistFields url i
			, youtubedlscraped = True
			}

{- Called when there is a problem with a feed.
 -
 - If the feed has been broken for some time,
 - returns False, otherwise only warns. -}
feedProblem :: URLString -> String -> Annex Bool
feedProblem url message = ifM (checkFeedBroken url)
	( do
		warning $ UnquotedString $ message ++ " (having repeated problems with feed: " ++ url ++ ")"
		return False
	, do
		warning $ UnquotedString $ "warning: " ++ message ++ " (feed: " ++ url ++ ")"
		return True
	)

{- A feed is only broken if problems have occurred repeatedly, for at
 - least 23 hours. -}
checkFeedBroken :: URLString -> Annex Bool
checkFeedBroken url = checkFeedBroken' url =<< feedState url
checkFeedBroken' :: URLString -> RawFilePath -> Annex Bool
checkFeedBroken' url f = do
	prev <- maybe Nothing readish
		<$> liftIO (catchMaybeIO $ readFile (fromRawFilePath f))
	now <- liftIO getCurrentTime
	case prev of
		Nothing -> do
			writeLogFile f $ show now
			return False
		Just prevtime -> do
			let broken = diffUTCTime now prevtime > 60 * 60 * 23
			when broken $
				-- Avoid repeatedly complaining about
				-- broken feed.
				clearFeedProblem url
			return broken

clearFeedProblem :: URLString -> Annex ()
clearFeedProblem url =
	void $ liftIO . tryIO . removeFile . fromRawFilePath
		=<< feedState url

feedState :: URLString -> Annex RawFilePath
feedState url = fromRepo $ gitAnnexFeedState $ fromUrl url Nothing False

{- The feed library parses the feed to Text, and does not use the
 - filesystem encoding to do it, so when the locale is not unicode
 - capable, a Text value can still include unicode characters. 
 -
 - So, it's not safe to use T.unpack to convert that to a String,
 - because later use of that String by eg encodeBS will crash
 - with an encoding error. Use this instead.
 -
 - This should not be used on a Text that is read using the
 - filesystem encoding because it does not reverse that encoding.
 -}
fromFeedText :: T.Text -> B.ByteString
fromFeedText = TE.encodeUtf8
