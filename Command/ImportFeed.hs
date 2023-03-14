{- git-annex command
 -
 - Copyright 2013-2021 Joey Hess <id@joeyh.name>
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
import Data.Char
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar
import Data.Time.LocalTime
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
import Command.AddUrl (addUrlFile, downloadRemoteFile, parseDownloadOptions, DownloadOptions(..), checkCanAdd)
import Annex.UUID
import Backend.URL (fromUrl)
import Annex.Content
import Annex.WorkTree
import Annex.YoutubeDl
import Types.MetaData
import Logs.MetaData
import Annex.MetaData
import Annex.FileMatcher
import Command.AddUrl (addWorkTree, checkRaw)
import Annex.UntrustedFilePath
import qualified Annex.Branch
import Logs
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = notBareRepo $ withAnnexOptions [backendOption] $
	command "importfeed" SectionCommon "import files from podcast feeds"
		(paramRepeating paramUrl) (seek <$$> optParser)

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
seek o = do
	addunlockedmatcher <- addUnlockedMatcher
	cache <- getCache (templateOption o)
	forM_ (feedUrls o) (getFeed addunlockedmatcher o cache)

getFeed :: AddUnlockedMatcher -> ImportFeedOptions -> Cache -> URLString -> CommandSeek
getFeed addunlockedmatcher opts cache url = do
	showStartOther "importfeed" (Just url) (SeekInput [])
	withTmpFile "feed" $ \tmpf h -> do
		liftIO $ hClose h
		ifM (downloadFeed url tmpf)
			( go tmpf
			, showEndResult =<< feedProblem url
				"downloading the feed failed"
			)
  where
	-- Use parseFeedFromFile rather than reading the file
	-- ourselves because it goes out of its way to handle encodings.
	go tmpf = liftIO (parseFeedFromFile' tmpf) >>= \case
		Nothing -> debugfeedcontent tmpf "parsing the feed failed"
		Just f -> do
			case map sanitizetitle $ decodeBS $ fromFeedText $ getFeedTitle f of
				"" -> noop
				t -> showNote ('"' : t ++ "\"")
			case findDownloads url f of
				[] -> debugfeedcontent tmpf "bad feed content; no enclosures to download"
				l -> do
					showEndOk
					ifM (and <$> mapM (performDownload addunlockedmatcher opts cache) l)
						( clearFeedProblem url
						, void $ feedProblem url 
							"problem downloading some item(s) from feed"
						)
	sanitizetitle c
		| isControl c = '_'
		| otherwise = c
	debugfeedcontent tmpf msg = do
		feedcontent <- liftIO $ readFile tmpf
		fastDebug "Command.ImportFeed" $ unlines
			[ "start of feed content"
			, feedcontent
			, "end of feed content"
			]
		showEndResult =<< feedProblem url
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
		showStart "importfeed" "gathering known urls" (SeekInput [])
		(us, is) <- knownItems
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

performDownload :: AddUnlockedMatcher -> ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
performDownload = performDownload' False

performDownload' :: Bool -> AddUnlockedMatcher -> ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
performDownload' started addunlockedmatcher opts cache todownload = case location todownload of
	Enclosure url -> checkknown url $ do
		starturl url
		rundownload url (takeWhile (/= '?') $ takeExtension url) $ \f -> do
			let f' = fromRawFilePath f
			r <- Remote.claimingUrl url
			if Remote.uuid r == webUUID || rawOption (downloadOptions opts)
				then checkRaw (Just url) (downloadOptions opts) Nothing $ do
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
								warning err
								return (Just [])
				else do
					res <- tryNonAsync $ maybe
						(error $ "unable to checkUrl of " ++ Remote.name r)
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
			= ifM forced (a, return True)
		| otherwise = a

	knownitemid = case getItemId (item todownload) of
		Just (_, itemid) ->
			S.member (decodeBS $ fromFeedText itemid) (knownitems cache)
		_ -> False

	rundownload url extension getter = do
		dest <- makeunique url (1 :: Integer) $
			feedFile (template cache) todownload extension
		case dest of
			Nothing -> return True
			Just f -> getter (toRawFilePath f) >>= \case
				Just ks
					-- Download problem.
					| null ks -> do
						showEndFail
						checkFeedBroken (feedurl todownload)
					| otherwise -> do
						forM_ ks $ \key ->
							ifM (annexGenMetaData <$> Annex.getGitConfig)
								( addMetaData key $ extractMetaData todownload
								, addMetaData key $ minimalMetaData todownload
								)
						showEndOk
						return True
				-- Was not able to add anything, but not
				-- because of a download problem.
				Nothing -> do
					showEndFail
					return False

	{- Find a unique filename to save the url to.
	 - If the file exists, prefixes it with a number.
	 - When forced, the file may already exist and have the same
	 - url, in which case Nothing is returned as it does not need
	 - to be re-downloaded. -}
	makeunique url n file = ifM alreadyexists
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
		tryanother = makeunique url (n + 1) file
		alreadyexists = liftIO $ isJust <$> catchMaybeIO (R.getSymbolicLinkStatus (toRawFilePath f))
		checksameurl k = ifM (elem url <$> getUrls k)
			( return Nothing
			, tryanother
			)
	
	downloadmedia linkurl mediaurl mediakey
		| rawOption (downloadOptions opts) = downloadlink False
		| otherwise = ifM (youtubeDlSupported linkurl)
			( do
				starturl linkurl
				r <- withTmpWorkDir mediakey $ \workdir -> do
					dl <- youtubeDl linkurl (fromRawFilePath workdir) nullMeterUpdate
					case dl of
						Right (Just mediafile) -> do
							let ext = case takeExtension mediafile of
								[] -> ".m"
								s -> s
							ok <- rundownload linkurl ext $ \f ->
								checkCanAdd (downloadOptions opts) f $ \canadd -> do
									addWorkTree canadd addunlockedmatcher webUUID mediaurl f mediakey (Just (toRawFilePath mediafile))
									return (Just [mediakey])
							return (Just ok)
						-- youtube-dl didn't support it, so
						-- download it as if the link were
						-- an enclosure.
						Right Nothing -> Just <$> downloadlink True
						Left msg -> do
							warning $ linkurl ++ ": " ++ msg
							return Nothing
				return (fromMaybe False r)
			, downloadlink False
			)
	  where
		downloadlink started' = checkRaw (Just linkurl) (downloadOptions opts) False $
			performDownload' started' addunlockedmatcher opts cache todownload
				{ location = Enclosure linkurl }

	addmediafast linkurl mediaurl mediakey =
		ifM (pure (not (rawOption (downloadOptions opts)))
		     <&&> youtubeDlSupported linkurl)
			( do
				starturl linkurl
				rundownload linkurl ".m" $ \f ->
					checkCanAdd (downloadOptions opts) f $ \canadd -> do
						addWorkTree canadd addunlockedmatcher webUUID mediaurl f mediakey Nothing
						return (Just [mediakey])
			, performDownload' started addunlockedmatcher opts cache todownload
				{ location = Enclosure linkurl }
			)

	starturl u = unless started $
		showStartOther "addurl" (Just u) (SeekInput [])

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
		warning $ message ++ " (having repeated problems with feed: " ++ url ++ ")"
		return False
	, do
		warning $ "warning: " ++ message
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
