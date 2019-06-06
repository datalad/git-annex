{- git-annex command
 -
 - Copyright 2013-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Command.ImportFeed where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Clock
import Data.Time.Format
#if ! MIN_VERSION_time(1,5,0)
import System.Locale
#endif
import qualified Data.Text as T

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
import Command.AddUrl (addWorkTree)

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
	cache <- getCache (templateOption o)
	forM_ (feedUrls o) (getFeed o cache)

getFeed :: ImportFeedOptions -> Cache -> URLString -> CommandSeek
getFeed opts cache url = do
	showStart "importfeed" url
	downloadFeed url >>= \case
		Nothing -> showEndResult =<< feedProblem url
			"downloading the feed failed"
		Just feedcontent -> case parseFeedString feedcontent of
			Nothing -> showEndResult =<< feedProblem url
				"parsing the feed failed"
			Just f -> case findDownloads url f of
				[] -> showEndResult =<< feedProblem url
					"bad feed content; no enclosures to download"
				l -> do
					showEndOk
					ifM (and <$> mapM (performDownload opts cache) l)
						( clearFeedProblem url
						, void $ feedProblem url 
							"problem downloading some item(s) from feed"
						)

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
		showStart "importfeed" "checking known urls"
		(is, us) <- unzip <$> (mapM knownItems =<< knownUrls)
		showEndOk
		ret (S.fromList us) (S.fromList (concat is))
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret us is = return $ Cache us is tmpl

knownItems :: (Key, URLString) -> Annex ([ItemId], URLString)
knownItems (k, u) = do
	itemids <- S.toList . S.filter (/= noneValue)
		. S.map (decodeBS . fromMetaValue)
		. currentMetaDataValues itemIdField 
		<$> getCurrentMetaData k
	return (itemids, u)

findDownloads :: URLString -> Feed -> [ToDownload]
findDownloads u f = catMaybes $ map mk (feedItems f)
  where
	mk i = case getItemEnclosure i of
		Just (enclosureurl, _, _) ->
			Just $ ToDownload f u i $ Enclosure $ 
				fromFeed enclosureurl
		Nothing -> case getItemLink i of
			Just link -> Just $ ToDownload f u i $ 
				MediaLink $ fromFeed link
			Nothing -> Nothing

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> Annex (Maybe String)
downloadFeed url
	| Url.parseURIRelaxed url == Nothing = giveup "invalid feed url"
	| otherwise = Url.withUrlOptions $ \uo ->
		liftIO $ withTmpFile "feed" $ \f h -> do
			hClose h
			ifM (Url.download nullMeterUpdate url f uo)
				( Just <$> readFileStrict f
				, return Nothing
				)

performDownload :: ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
performDownload opts cache todownload = case location todownload of
	Enclosure url -> checkknown url $
		rundownload url (takeWhile (/= '?') $ takeExtension url) $ \f -> do
			r <- Remote.claimingUrl url
			if Remote.uuid r == webUUID || rawOption (downloadOptions opts)
				then do
					urlinfo <- if relaxedOption (downloadOptions opts)
						then pure Url.assumeUrlExists
						else Url.withUrlOptions $
							liftIO . Url.getUrlInfo url
					let dlopts = (downloadOptions opts)
						-- force using the filename
						-- chosen here
						{ fileOption = Just f
						-- don't use youtube-dl
						, rawOption = True
						}
					maybeToList <$> addUrlFile dlopts url urlinfo f
				else do
					res <- tryNonAsync $ maybe
						(error $ "unable to checkUrl of " ++ Remote.name r)
						(flip id url)
						(Remote.checkUrl r)
					case res of
						Left _ -> return []
						Right (UrlContents sz _) ->
							maybeToList <$>
								downloadRemoteFile r (downloadOptions opts) url f sz
						Right (UrlMulti l) -> do
							kl <- forM l $ \(url', sz, subf) ->
								downloadRemoteFile r (downloadOptions opts) url' (f </> fromSafeFilePath subf) sz
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
			S.member (fromFeed itemid) (knownitems cache)
		_ -> False

	rundownload url extension getter = do
		dest <- makeunique url (1 :: Integer) $
			feedFile (template cache) todownload extension
		case dest of
			Nothing -> return True
			Just f -> do
				showStart "addurl" url
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
			( ifAnnexed f checksameurl tryanother
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
							addWorkTree webUUID mediaurl f mediakey (Just mediafile)
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
		downloadlink = performDownload opts cache todownload
			{ location = Enclosure linkurl }

	addmediafast linkurl mediaurl mediakey =
		ifM (pure (not (rawOption (downloadOptions opts)))
		     <&&> youtubeDlSupported linkurl)
			( rundownload linkurl ".m" $ \f -> do
				addWorkTree webUUID mediaurl f mediakey Nothing
				return [mediakey]
			, performDownload opts cache todownload
				{ location = Enclosure linkurl }
			)

defaultTemplate :: String
defaultTemplate = "${feedtitle}/${itemtitle}${extension}"

{- Generates a filename to use for a feed item by filling out the template.
 - The filename may not be unique. -}
feedFile :: Utility.Format.Format -> ToDownload -> String -> FilePath
feedFile tmpl i extension = Utility.Format.format tmpl $
	M.map sanitizeFilePath $ M.fromList $ extractFields i ++
		[ ("extension", extension)
		, extractField "itempubdate" [pubdate $ item i]
		]
  where
	pubdate itm = case getItemPublishDate itm :: Maybe (Maybe UTCTime) of
		Just (Just d) -> Just $
			formatTime defaultTimeLocale "%F" d
		-- if date cannot be parsed, use the raw string
		_ -> replace "/" "-" . fromFeed
			<$> getItemPublishDateString itm

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
		(S.singleton $ toMetaValue $ encodeBS $ fromFeed itemid)

{- Extract fields from the feed and item, that are both used as metadata,
 - and to generate the filename. -}
extractFields :: ToDownload -> [(String, String)]
extractFields i = map (uncurry extractField)
	[ ("feedtitle", [feedtitle])
	, ("itemtitle", [itemtitle])
	, ("feedauthor", [feedauthor])
	, ("itemauthor", [itemauthor])
	, ("itemsummary", [fromFeed <$> getItemSummary (item i)])
	, ("itemdescription", [fromFeed <$> getItemDescription (item i)])
	, ("itemrights", [fromFeed <$> getItemRights (item i)])
	, ("itemid", [fromFeed . snd <$> getItemId (item i)])
	, ("title", [itemtitle, feedtitle])
	, ("author", [itemauthor, feedauthor])
	]
  where
	feedtitle = Just $ fromFeed $ getFeedTitle $ feed i
	itemtitle = fromFeed <$> getItemTitle (item i)
	feedauthor = fromFeed <$> getFeedAuthor (feed i)
	itemauthor = fromFeed <$> getItemAuthor (item i)

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

#if MIN_VERSION_feed(1,0,0)
fromFeed :: T.Text -> String
fromFeed = T.unpack
#else
fromFeed :: String -> String
fromFeed = id
#endif
