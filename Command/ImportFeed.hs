{- git-annex command
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import qualified Data.Text as T
import System.Log.Logger
import Control.Concurrent.Async

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
import Command.AddUrl (addUrlFile, downloadRemoteFile, parseDownloadOptions, DownloadOptions(..))
import Annex.UUID
import Backend.URL (fromUrl)
import Annex.Content
import Annex.YoutubeDl
import Types.MetaData
import Logs.MetaData
import Annex.MetaData
import Annex.FileMatcher
import Command.AddUrl (addWorkTree)
import Annex.UntrustedFilePath
import qualified Git.Ref
import qualified Annex.Branch
import Logs
import Git.CatFile (catObjectStream)

cmd :: Command
cmd = notBareRepo $
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
	downloadFeed url >>= \case
		Nothing -> showEndResult =<< feedProblem url
			"downloading the feed failed"
		Just feedcontent -> case parseFeedString feedcontent of
			Nothing -> debugfeedcontent feedcontent "parsing the feed failed"
			Just f -> case findDownloads url f of
				[] -> debugfeedcontent feedcontent "bad feed content; no enclosures to download"
				l -> do
					showEndOk
					ifM (and <$> mapM (performDownload addunlockedmatcher opts cache) l)
						( clearFeedProblem url
						, void $ feedProblem url 
							"problem downloading some item(s) from feed"
						)
  where
	debugfeedcontent feedcontent msg = do
		liftIO $ debugM "feed content" $ unlines
			[ "start of feed content"
			, feedcontent
			, "end of feed content"
			]
		showEndResult =<< feedProblem url
			(msg ++ " (use --debug to see the feed content that was downloaded)")

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
getCache opttemplate = ifM (Annex.getState Annex.force)
	( ret S.empty S.empty
	, do
		showStart "importfeed" "checking known urls" (SeekInput [])
		(is, us) <- unzip <$> knownItems
		showEndOk
		ret (S.fromList us) (S.fromList (concat is))
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret us is = return $ Cache us is tmpl

knownItems :: Annex [([ItemId], URLString)]
knownItems = do
	g <- Annex.gitRepo
	config <- Annex.getGitConfig
	catObjectStream g $ \catfeeder catcloser catreader -> do
		rt <- liftIO $ async $ reader catreader []
		withKnownUrls (feeder config catfeeder catcloser)
		liftIO (wait rt)
  where
	feeder config catfeeder catcloser urlreader = urlreader >>= \case
		Just (k, us) -> do
			forM_ us $ \u ->
				let logf = metaDataLogFile config k
				    ref = Git.Ref.branchFileRef Annex.Branch.fullname logf
				in liftIO $ catfeeder (u, ref)
			feeder config catfeeder catcloser urlreader
		Nothing -> liftIO catcloser
	
	reader catreader c = catreader >>= \case
		Just (u, Just mdc) ->
			let !itemids = S.toList $ S.filter (/= noneValue) $
				S.map (decodeBS . fromMetaValue) $
					currentMetaDataValues itemIdField $
						parseCurrentMetaData mdc
			in reader catreader ((itemids,u):c)
		Just (u, Nothing) -> reader catreader (([],u):c)
		Nothing -> return c

findDownloads :: URLString -> Feed -> [ToDownload]
findDownloads u f = catMaybes $ map mk (feedItems f)
  where
	mk i = case getItemEnclosure i of
		Just (enclosureurl, _, _) ->
			Just $ ToDownload f u i $ Enclosure $ 
				T.unpack enclosureurl
		Nothing -> case getItemLink i of
			Just l -> Just $ ToDownload f u i $ 
				MediaLink $ T.unpack l
			Nothing -> Nothing

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> Annex (Maybe String)
downloadFeed url
	| Url.parseURIRelaxed url == Nothing = giveup "invalid feed url"
	| otherwise = withTmpFile "feed" $ \f h -> do
		liftIO $ hClose h
		ifM (Url.withUrlOptions $ Url.download nullMeterUpdate url f)
			( Just <$> liftIO (readFileStrict f)
			, return Nothing
			)

performDownload :: AddUnlockedMatcher -> ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
performDownload addunlockedmatcher opts cache todownload = case location todownload of
	Enclosure url -> checkknown url $
		rundownload url (takeWhile (/= '?') $ takeExtension url) $ \f -> do
			r <- Remote.claimingUrl url
			if Remote.uuid r == webUUID || rawOption (downloadOptions opts)
				then do
					let dlopts = (downloadOptions opts)
						-- force using the filename
						-- chosen here
						{ fileOption = Just f
						-- don't use youtube-dl
						, rawOption = True
						}
					let go urlinfo = maybeToList <$> addUrlFile addunlockedmatcher dlopts url urlinfo f
					if relaxedOption (downloadOptions opts)
						then go Url.assumeUrlExists
						else Url.withUrlOptions (Url.getUrlInfo url) >>= \case
							Right urlinfo -> go urlinfo
							Left err -> do
								warning err
								return []
				else do
					res <- tryNonAsync $ maybe
						(error $ "unable to checkUrl of " ++ Remote.name r)
						(flip id url)
						(Remote.checkUrl r)
					case res of
						Left _ -> return []
						Right (UrlContents sz _) ->
							maybeToList <$>
								downloadRemoteFile addunlockedmatcher r (downloadOptions opts) url f sz
						Right (UrlMulti l) -> do
							kl <- forM l $ \(url', sz, subf) ->
								downloadRemoteFile addunlockedmatcher r (downloadOptions opts) url' (f </> sanitizeFilePath subf) sz
							return $ if all isJust kl
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
			ifM (Annex.getState Annex.fast <||> pure (relaxedOption (downloadOptions opts)))
				( addmediafast linkurl mediaurl mediakey
				, downloadmedia linkurl mediaurl mediakey
				)
  where
	forced = Annex.getState Annex.force

	{- Avoids downloading any items that are already known to be
	 - associated with a file in the annex, unless forced. -}
	checkknown url a
		| knownitemid || S.member url (knownurls cache)
			= ifM forced (a, return True)
		| otherwise = a

	knownitemid = case getItemId (item todownload) of
		Just (_, itemid) ->
			S.member (T.unpack itemid) (knownitems cache)
		_ -> False

	rundownload url extension getter = do
		dest <- makeunique url (1 :: Integer) $
			feedFile (template cache) todownload extension
		case dest of
			Nothing -> return True
			Just f -> do
				showStartOther "addurl" (Just url) (SeekInput [])
				ks <- getter f
				if null ks
					then do
						showEndFail
						checkFeedBroken (feedurl todownload)
					else do
						forM_ ks $ \key ->
							ifM (annexGenMetaData <$> Annex.getGitConfig)
								( addMetaData key $ extractMetaData todownload
								, addMetaData key $ minimalMetaData todownload
								)
						showEndOk
						return True

	{- Find a unique filename to save the url to.
	 - If the file exists, prefixes it with a number.
	 - When forced, the file may already exist and have the same
	 - url, in which case Nothing is returned as it does not need
	 - to be re-downloaded. -}
	makeunique url n file = ifM alreadyexists
		( ifM forced
			( ifAnnexed (toRawFilePath f) checksameurl tryanother
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
		alreadyexists = liftIO $ isJust <$> catchMaybeIO (getSymbolicLinkStatus f)
		checksameurl k = ifM (elem url <$> getUrls k)
			( return Nothing
			, tryanother
			)
	
	downloadmedia linkurl mediaurl mediakey
		| rawOption (downloadOptions opts) = downloadlink
		| otherwise = do
			r <- withTmpWorkDir mediakey $ \workdir -> do
				dl <- youtubeDl linkurl workdir
				case dl of
					Right (Just mediafile) -> do
						let ext = case takeExtension mediafile of
							[] -> ".m"
							s -> s
						ok <- rundownload linkurl ext $ \f -> do
							addWorkTree addunlockedmatcher webUUID mediaurl f mediakey (Just mediafile)
							return [mediakey]
						return (Just ok)
					-- youtude-dl didn't support it, so
					-- download it as if the link were
					-- an enclosure.
					Right Nothing -> Just <$> downloadlink
					Left msg -> do
						warning msg
						return Nothing
			return (fromMaybe False r)
	  where
		downloadlink = performDownload addunlockedmatcher opts cache todownload
			{ location = Enclosure linkurl }

	addmediafast linkurl mediaurl mediakey =
		ifM (pure (not (rawOption (downloadOptions opts)))
		     <&&> youtubeDlSupported linkurl)
			( rundownload linkurl ".m" $ \f -> do
				addWorkTree addunlockedmatcher webUUID mediaurl f mediakey Nothing
				return [mediakey]
			, performDownload addunlockedmatcher opts cache todownload
				{ location = Enclosure linkurl }
			)

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
		Nothing-> replace "/" "-" . T.unpack
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
		(S.singleton $ toMetaValue $ encodeBS $ T.unpack itemid)

{- Extract fields from the feed and item, that are both used as metadata,
 - and to generate the filename. -}
extractFields :: ToDownload -> [(String, String)]
extractFields i = map (uncurry extractField)
	[ ("feedtitle", [feedtitle])
	, ("itemtitle", [itemtitle])
	, ("feedauthor", [feedauthor])
	, ("itemauthor", [itemauthor])
	, ("itemsummary", [T.unpack <$> getItemSummary (item i)])
	, ("itemdescription", [T.unpack <$> getItemDescription (item i)])
	, ("itemrights", [T.unpack <$> getItemRights (item i)])
	, ("itemid", [T.unpack . snd <$> getItemId (item i)])
	, ("title", [itemtitle, feedtitle])
	, ("author", [itemauthor, feedauthor])
	]
  where
	feedtitle = Just $ T.unpack $ getFeedTitle $ feed i
	itemtitle = T.unpack <$> getItemTitle (item i)
	feedauthor = T.unpack <$> getFeedAuthor (feed i)
	itemauthor = T.unpack <$> getItemAuthor (item i)

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
checkFeedBroken' :: URLString -> FilePath -> Annex Bool
checkFeedBroken' url f = do
	prev <- maybe Nothing readish <$> liftIO (catchMaybeIO $ readFile f)
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
clearFeedProblem url = void $ liftIO . tryIO . removeFile =<< feedState url

feedState :: URLString -> Annex FilePath
feedState url = fromRepo $ gitAnnexFeedState $ fromUrl url Nothing
