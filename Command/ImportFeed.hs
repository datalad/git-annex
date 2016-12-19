{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

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

import Command
import qualified Annex
import qualified Annex.Url as Url
import qualified Remote
import qualified Types.Remote as Remote
import Types.UrlContents
import Logs.Web
import qualified Utility.Format
import Utility.Tmp
import Command.AddUrl (addUrlFile, downloadRemoteFile, parseRelaxedOption, parseRawOption)
import Annex.Perms
import Annex.UUID
import Backend.URL (fromUrl)
import Annex.Quvi
import qualified Utility.Quvi as Quvi
import Command.AddUrl (addUrlFileQuvi)
import Types.MetaData
import Logs.MetaData
import Annex.MetaData

cmd :: Command
cmd = notBareRepo $
	command "importfeed" SectionCommon "import files from podcast feeds"
		(paramRepeating paramUrl) (seek <$$> optParser)

data ImportFeedOptions = ImportFeedOptions
	{ feedUrls :: CmdParams
	, templateOption :: Maybe String
	, relaxedOption :: Bool
	, rawOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser ImportFeedOptions
optParser desc = ImportFeedOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "template" <> metavar paramFormat
		<> help "template for filenames"
		))
	<*> parseRelaxedOption
	<*> parseRawOption

seek :: ImportFeedOptions -> CommandSeek
seek o = do
	cache <- getCache (templateOption o)
	withStrings (start o cache) (feedUrls o)

start :: ImportFeedOptions -> Cache -> URLString -> CommandStart
start opts cache url = do
	showStart "importfeed" url
	next $ perform opts cache url

perform :: ImportFeedOptions -> Cache -> URLString -> CommandPerform
perform opts cache url = do
	v <- findDownloads url
	case v of
		[] -> do
			feedProblem url "bad feed content; no enclosures to download"
			next $ return True
		l -> do
			ok <- and <$> mapM (performDownload opts cache) l
			unless ok $
				feedProblem url "problem downloading item"
			next $ cleanup url True

cleanup :: URLString -> Bool -> CommandCleanup
cleanup url ok = do
	when ok $
		clearFeedProblem url
	return ok

data ToDownload = ToDownload
	{ feed :: Feed
	, feedurl :: URLString
	, item :: Item
	, location :: DownloadLocation
	}

data DownloadLocation = Enclosure URLString | QuviLink URLString

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
		showAction "checking known urls"
		(is, us) <- unzip <$> (mapM knownItems =<< knownUrls)
		ret (S.fromList us) (S.fromList (concat is))
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret us is = return $ Cache us is tmpl

knownItems :: (Key, URLString) -> Annex ([ItemId], URLString)
knownItems (k, u) = do
	itemids <- S.toList . S.filter (/= noneValue) . S.map fromMetaValue 
		. currentMetaDataValues itemIdField 
		<$> getCurrentMetaData k
	return (itemids, u)

findDownloads :: URLString -> Annex [ToDownload]
findDownloads u = go =<< downloadFeed u
  where
	go Nothing = pure []
	go (Just f) = catMaybes <$> mapM (mk f) (feedItems f)

	mk f i = case getItemEnclosure i of
		Just (enclosureurl, _, _) -> return $ 
			Just $ ToDownload f u i $ Enclosure enclosureurl
		Nothing -> mkquvi f i
	mkquvi f i = case getItemLink i of
		Just link -> do
			liftIO $ print ("link", link)
			ifM (quviSupported link)
				( return $ Just $ ToDownload f u i $ QuviLink link
				, return Nothing
				)
		Nothing -> return Nothing

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> Annex (Maybe Feed)
downloadFeed url
	| Url.parseURIRelaxed url == Nothing = giveup "invalid feed url"
	| otherwise = do
		showOutput
		uo <- Url.getUrlOptions
		liftIO $ withTmpFile "feed" $ \f h -> do
			hClose h
			ifM (Url.download url f uo)
				( parseFeedString <$> readFileStrictAnyEncoding f
				, return Nothing
				)

performDownload :: ImportFeedOptions -> Cache -> ToDownload -> Annex Bool
performDownload opts cache todownload = case location todownload of
	Enclosure url -> checkknown url $
		rundownload url (takeWhile (/= '?') $ takeExtension url) $ \f -> do
			r <- Remote.claimingUrl url
			if Remote.uuid r == webUUID || rawOption opts
				then do
					urlinfo <- if relaxedOption opts
						then pure Url.assumeUrlExists
						else Url.withUrlOptions (Url.getUrlInfo url)
					maybeToList <$> addUrlFile (relaxedOption opts) url urlinfo f
				else do
					res <- tryNonAsync $ maybe
						(error $ "unable to checkUrl of " ++ Remote.name r)
						(flip id url)
						(Remote.checkUrl r)
					case res of
						Left _ -> return []
						Right (UrlContents sz _) ->
							maybeToList <$>
								downloadRemoteFile r (relaxedOption opts) url f sz
						Right (UrlMulti l) -> do
							kl <- forM l $ \(url', sz, subf) ->
								downloadRemoteFile r (relaxedOption opts) url' (f </> fromSafeFilePath subf) sz
							return $ if all isJust kl
								then catMaybes kl
								else []
							
	QuviLink pageurl -> do
		let quviurl = setDownloader pageurl QuviDownloader
		checkknown quviurl $ do
			mp <- withQuviOptions Quvi.query [Quvi.quiet, Quvi.httponly] pageurl
			case mp of
				Nothing -> return False
				Just page -> case headMaybe $ Quvi.pageLinks page of
					Nothing -> return False
					Just link -> do
						let videourl = Quvi.linkUrl link
						checkknown videourl $
							rundownload videourl ("." ++ fromMaybe "m" (Quvi.linkSuffix link)) $ \f ->
								maybeToList <$> addUrlFileQuvi (relaxedOption opts) quviurl videourl f
  where
	forced = Annex.getState Annex.force

	{- Avoids downloading any items that are already known to be
	 - associated with a file in the annex, unless forced. -}
	checkknown url a
		| knownitemid || S.member url (knownurls cache)
			= ifM forced (a, return True)
		| otherwise = a

	knownitemid = case getItemId (item todownload) of
		Just (_, itemid) -> S.member itemid (knownitems cache)
		_ -> False

	rundownload url extension getter = do
		dest <- makeunique url (1 :: Integer) $
			feedFile (template cache) todownload extension
		case dest of
			Nothing -> return True
			Just f -> do
				showStart "addurl" f
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
#if MIN_VERSION_feed(0,3,9)
	pubdate itm = case getItemPublishDate itm :: Maybe (Maybe UTCTime) of
		Just (Just d) -> Just $
			formatTime defaultTimeLocale "%F" d
		-- if date cannot be parsed, use the raw string
		_ -> replace "/" "-" <$> getItemPublishDateString itm
#else
	pubdate _ = Nothing
#endif

extractMetaData :: ToDownload -> MetaData
#if MIN_VERSION_feed(0,3,9)
extractMetaData i = case getItemPublishDate (item i) :: Maybe (Maybe UTCTime) of
	Just (Just d) -> unionMetaData meta (dateMetaData d meta)
	_ -> meta
#else
extractMetaData i = meta
#endif
  where
	tometa (k, v) = (mkMetaFieldUnchecked k, S.singleton (toMetaValue v))
	meta = MetaData $ M.fromList $ map tometa $ extractFields i

minimalMetaData :: ToDownload -> MetaData
minimalMetaData i = case getItemId (item i) of
	(Nothing) -> emptyMetaData
	(Just (_, itemid)) -> MetaData $ M.singleton itemIdField 
		(S.singleton $ toMetaValue itemid)

{- Extract fields from the feed and item, that are both used as metadata,
 - and to generate the filename. -}
extractFields :: ToDownload -> [(String, String)]
extractFields i = map (uncurry extractField)
	[ ("feedtitle", [feedtitle])
	, ("itemtitle", [itemtitle])
	, ("feedauthor", [feedauthor])
	, ("itemauthor", [itemauthor])
	, ("itemsummary", [getItemSummary $ item i])
	, ("itemdescription", [getItemDescription $ item i])
	, ("itemrights", [getItemRights $ item i])
	, ("itemid", [snd <$> getItemId (item i)])
	, ("title", [itemtitle, feedtitle])
	, ("author", [itemauthor, feedauthor])
	]
  where
	feedtitle = Just $ getFeedTitle $ feed i
	itemtitle = getItemTitle $ item i
	feedauthor = getFeedAuthor $ feed i
	itemauthor = getItemAuthor $ item i

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
 - Throws an error if the feed is broken, otherwise shows a warning. -}
feedProblem :: URLString -> String -> Annex ()
feedProblem url message = ifM (checkFeedBroken url)
	( giveup $ message ++ " (having repeated problems with feed: " ++ url ++ ")"
	, warning $ "warning: " ++ message
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
			createAnnexDirectory (parentDir f)
			liftIO $ writeFile f $ show now
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
