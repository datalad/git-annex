{- git-annex command
 -
 - Copyright 2013-2023 Joey Hess <id@joeyh.name>
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
import Command.AddUrl (addUrlFile, downloadRemoteFile, parseDownloadOptions, DownloadOptions(..), checkCanAdd, addWorkTree, checkRaw)
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
import qualified Annex.Branch
import Logs
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = notBareRepo $ withAnnexOptions os $
	command "importfeed" SectionCommon "import files from podcast feeds"
		(paramRepeating paramUrl) (seek <$$> optParser)
  where
	os = [jobsOption, jsonOptions, jsonProgressOption, backendOption]

data ImportFeedOptions = ImportFeedOptions
	{ feedUrls :: CmdParams
	, templateOption :: Maybe String
	, downloadOptions :: DownloadOptions
	}

optParser :: CmdParamsDesc -> Parser ImportFeedOptions
optParser desc = ImportFeedOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "template" <> metavar paramFormat
		<> help "template for filenames"
		))
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
		commandAction $ getFeed url dlst
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
					in if M.null pending
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
	:: URLString
	-> TMVar (M.Map URLString (Maybe (Maybe [ToDownload])))
	-> CommandStart
getFeed url st =
	starting "importfeed" (ActionItemOther (Just (UnquotedString url))) (SeekInput [url]) $
		get `onException` recordfail
  where
	record v = liftIO $ atomically $ do
		m <- takeTMVar st
		putTMVar st (M.insert url v m)
	recordfail = record (Just Nothing)
	
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

parseFeedFromFile' :: FilePath -> IO (Maybe Feed)
#if MIN_VERSION_feed(1,1,0)
parseFeedFromFile' = parseFeedFromFile
#else
parseFeedFromFile' f = catchMaybeIO (parseFeedFromFile f)
#endif

data ToDownload = ToDownload
	{ feed :: Feed
	, feedurl :: URLString
	, item :: Item
	, location :: DownloadLocation
	}

data DownloadLocation = Enclosure URLString | MediaLink URLString

type ItemId = String

data Cache = Cache
	{ knownurls :: S.Set URLString
	, knownitems :: S.Set ItemId
	, template :: Utility.Format.Format
	}

getCache :: Maybe String -> Annex Cache
getCache opttemplate = ifM (Annex.getRead Annex.force)
	( ret S.empty S.empty
	, do
		j <- jsonOutputEnabled
		unless j $
			showStartMessage (StartMessage "importfeed" (ActionItemOther (Just "gathering known urls")) (SeekInput []))
		(us, is) <- knownItems
		unless j
			showEndOk
		ret (S.fromList us) (S.fromList is)
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret us is = return $ Cache us is tmpl

{- Scan all url logs and metadata logs in the branch and find urls
 - and ItemIds that are already known. -}
knownItems :: Annex ([URLString], [ItemId])
knownItems = Annex.Branch.overBranchFileContents select (go [] []) >>= \case
		Just r -> return r
		Nothing -> giveup "This repository is read-only."
  where
	select f
		| isUrlLog f = Just ()
		| isMetaDataLog f = Just ()
		| otherwise = Nothing

	go uc ic reader = reader >>= \case
		Just ((), f, Just content)
			| isUrlLog f -> case parseUrlLog content of
				[] -> go uc ic reader
				us -> go (us++uc) ic reader
			| isMetaDataLog f ->
				let s = currentMetaDataValues itemIdField $
					parseCurrentMetaData content
				in if S.null s
					then go uc ic reader
					else go uc (map (decodeBS . fromMetaValue) (S.toList s)++ic) reader
			| otherwise -> go uc ic reader
		Just ((), _, Nothing) -> go uc ic reader
		Nothing -> return (uc, ic)

findDownloads :: URLString -> Feed -> [ToDownload]
findDownloads u f = catMaybes $ map mk (feedItems f)
  where
	mk i = case getItemEnclosure i of
		Just (enclosureurl, _, _) ->
			Just $ ToDownload f u i $ Enclosure $ 
				decodeBS $ fromFeedText enclosureurl
		Nothing -> case getItemLink i of
			Just l -> Just $ ToDownload f u i $ 
				MediaLink $ decodeBS $ fromFeedText l
			Nothing -> Nothing

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
		let mediakey = Backend.URL.fromUrl mediaurl Nothing
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
	forced = Annex.getRead Annex.force

	{- Avoids downloading any items that are already known to be
	 - associated with a file in the annex, unless forced. -}
	checkknown url a
		| knownitemid || S.member url (knownurls cache)
			= ifM forced (a, nothingtodo)
		| otherwise = a

	nothingtodo = recordsuccess >> stop

	recordsuccess = liftIO $ atomically $ putTMVar cv True
	
	startdownloadenclosure :: URLString -> CommandStart
	startdownloadenclosure url = checkknown url $ startUrlDownload cv todownload url $
		downloadEnclosure addunlockedmatcher opts cache cv todownload url 

	knownitemid = case getItemId (item todownload) of
		Just (_, itemid) ->
			S.member (decodeBS $ fromFeedText itemid) (knownitems cache)
		_ -> False

	downloadmedia linkurl mediaurl mediakey
		| rawOption (downloadOptions opts) = startdownloadlink
		| otherwise = ifM (youtubeDlSupported linkurl)
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
		ifM (pure (not (rawOption (downloadOptions opts)))
		     <&&> youtubeDlSupported linkurl)
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
		r <- Remote.claimingUrl url
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
			stop
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
		checksameurl k = ifM (elem url <$> getUrls k)
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
		M.map sanitizeFilePathComponent $ M.fromList $ extractFields i ++
			[ ("extension", extension)
			, extractField "itempubdate" [itempubdate]
			, extractField "itempubyear" [itempubyear]
			, extractField "itempubmonth" [itempubmonth]
			, extractField "itempubday" [itempubday]
			, extractField "itempubhour" [itempubhour]
			, extractField "itempubminute" [itempubminute]
			, extractField "itempubsecond" [itempubsecond]
			]
  where
	itm = item i

	pubdate = case getItemPublishDate itm :: Maybe (Maybe UTCTime) of
		Just (Just d) -> Just d
		_ -> Nothing
	
	itempubdate = case pubdate of
		Just pd -> Just $
			formatTime defaultTimeLocale "%F" pd
		-- if date cannot be parsed, use the raw string
		Nothing-> replace "/" "-" . decodeBS . fromFeedText
			<$> getItemPublishDateString itm
	
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
extractMetaData i = case getItemPublishDate (item i) :: Maybe (Maybe UTCTime) of
	Just (Just d) -> unionMetaData meta (dateMetaData d meta)
	_ -> meta
  where
	tometa (k, v) = (mkMetaFieldUnchecked (T.pack k), S.singleton (toMetaValue (encodeBS v)))
	meta = MetaData $ M.fromList $ map tometa $ extractFields i

minimalMetaData :: ToDownload -> MetaData
minimalMetaData i = case getItemId (item i) of
	(Nothing) -> emptyMetaData
	(Just (_, itemid)) -> MetaData $ M.singleton itemIdField 
		(S.singleton $ toMetaValue $fromFeedText itemid)

{- Extract fields from the feed and item, that are both used as metadata,
 - and to generate the filename. -}
extractFields :: ToDownload -> [(String, String)]
extractFields i = map (uncurry extractField)
	[ ("feedtitle", [feedtitle])
	, ("itemtitle", [itemtitle])
	, ("feedauthor", [feedauthor])
	, ("itemauthor", [itemauthor])
	, ("itemsummary", [decodeBS . fromFeedText <$> getItemSummary (item i)])
	, ("itemdescription", [decodeBS . fromFeedText <$> getItemDescription (item i)])
	, ("itemrights", [decodeBS . fromFeedText <$> getItemRights (item i)])
	, ("itemid", [decodeBS . fromFeedText . snd <$> getItemId (item i)])
	, ("title", [itemtitle, feedtitle])
	, ("author", [itemauthor, feedauthor])
	]
  where
	feedtitle = Just $ decodeBS $ fromFeedText $ getFeedTitle $ feed i
	itemtitle = decodeBS . fromFeedText <$> getItemTitle (item i)
	feedauthor = decodeBS . fromFeedText <$> getFeedAuthor (feed i)
	itemauthor = decodeBS . fromFeedText <$> getItemAuthor (item i)

itemIdField :: MetaField
itemIdField = mkMetaFieldUnchecked "itemid"

extractField :: String -> [Maybe String] -> (String, String)
extractField k [] = (k, noneValue)
extractField k (Just v:_)
	| not (null v) = (k, v)
extractField k (_:rest) = extractField k rest

noneValue :: String
noneValue = "none"

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
feedState url = fromRepo $ gitAnnexFeedState $ fromUrl url Nothing

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
