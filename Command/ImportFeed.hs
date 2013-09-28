{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.ImportFeed where

import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Clock

import Common.Annex
import qualified Annex
import Command
import qualified Annex.Url as Url
import Logs.Web
import qualified Option
import qualified Utility.Format
import Utility.Tmp
import Command.AddUrl (addUrlFile, relaxedOption)
import Annex.Perms
import Backend.URL (fromUrl)

def :: [Command]
def = [notBareRepo $ withOptions [templateOption, relaxedOption] $
	command "importfeed" (paramRepeating paramUrl) seek
		SectionCommon "import files from podcast feeds"]

templateOption :: Option
templateOption = Option.field [] "template" paramFormat "template for filenames"

seek :: [CommandSeek]
seek = [withField templateOption return $ \tmpl ->
	withFlag relaxedOption $ \relaxed ->
	withValue (getCache tmpl) $ \cache ->
	withStrings $ start relaxed cache]

start :: Bool -> Cache -> URLString -> CommandStart
start relaxed cache url = do
	showStart "importfeed" url
	next $ perform relaxed cache url

perform :: Bool -> Cache -> URLString -> CommandPerform
perform relaxed cache url = do
	v <- findEnclosures url
	case v of
		Just l | not (null l) -> do
			ok <- and <$> mapM (downloadEnclosure relaxed cache) l
			unless ok $
				feedProblem url "problem downloading item"
			next $ cleanup url True
		_ -> do
			feedProblem url "bad feed content"
			next $ return True

cleanup :: URLString -> Bool -> CommandCleanup
cleanup url ok = do
	when ok $
		clearFeedProblem url
	return ok

data ToDownload = ToDownload
	{ feed :: Feed
	, feedurl :: URLString
	, item :: Item
	, location :: URLString
	}

mkToDownload :: Feed -> URLString -> Item -> Maybe ToDownload
mkToDownload f u i = case getItemEnclosure i of
	Nothing -> Nothing
	Just (enclosureurl, _, _) -> Just $ ToDownload f u i enclosureurl

data Cache = Cache
	{ knownurls :: S.Set URLString
	, template :: Utility.Format.Format
	}

getCache :: Maybe String -> Annex Cache
getCache opttemplate = ifM (Annex.getState Annex.force)
	( ret S.empty
	, do
		showSideAction "checking known urls"
		ret =<< S.fromList <$> knownUrls
	)
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate
	ret s = return $ Cache s tmpl

findEnclosures :: URLString -> Annex (Maybe [ToDownload])
findEnclosures url = extract <$> downloadFeed url
  where
	extract Nothing = Nothing
	extract (Just f) = Just $ mapMaybe (mkToDownload f url) (feedItems f)

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> Annex (Maybe Feed)
downloadFeed url = do
	showOutput
	ua <- Url.getUserAgent
	liftIO $ withTmpFile "feed" $ \f h -> do
		fileEncoding h
		ifM (Url.download url [] [] f ua)
			( liftIO $ parseFeedString <$> hGetContentsStrict h
			, return Nothing
			)

{- Avoids downloading any urls that are already known to be associated
 - with a file in the annex, unless forced. -}
downloadEnclosure :: Bool -> Cache -> ToDownload -> Annex Bool
downloadEnclosure relaxed cache enclosure
	| S.member url (knownurls cache) = ifM forced (go, return True)
	| otherwise = go
  where
  	forced = Annex.getState Annex.force
  	url = location enclosure
	go = do
		dest <- makeunique (1 :: Integer) $ feedFile (template cache) enclosure
		case dest of
			Nothing -> return True
			Just f -> do
				showStart "addurl" f
				ok <- addUrlFile relaxed url f
				if ok
					then do
						showEndOk
						return True
					else do
						showEndFail
						checkFeedBroken (feedurl enclosure)
	{- Find a unique filename to save the url to.
	 - If the file exists, prefixes it with a number.
	 - When forced, the file may already exist and have the same
	 - url, in which case Nothing is returned as it does not need
	 - to be re-downloaded. -}
	makeunique n file = ifM alreadyexists
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
		tryanother = makeunique (n + 1) file
		alreadyexists = liftIO $ isJust <$> catchMaybeIO (getSymbolicLinkStatus f)
		checksameurl (k, _) = ifM (elem url <$> getUrls k)
			( return Nothing
			, tryanother
			)
	
defaultTemplate :: String
defaultTemplate = "${feedtitle}/${itemtitle}${extension}"

{- Generates a filename to use for a feed item by filling out the template.
 - The filename may not be unique. -}
feedFile :: Utility.Format.Format -> ToDownload -> FilePath
feedFile tmpl i = Utility.Format.format tmpl $ M.fromList
	[ field "feedtitle" $ getFeedTitle $ feed i
	, fieldMaybe "itemtitle" $ getItemTitle $ item i
	, fieldMaybe "feedauthor" $ getFeedAuthor $ feed i
	, fieldMaybe "itemauthor" $ getItemAuthor $ item i
	, fieldMaybe "itemsummary" $ getItemSummary $ item i
	, fieldMaybe "itemdescription" $ getItemDescription $ item i
	, fieldMaybe "itemrights" $ getItemRights $ item i
	, fieldMaybe "itemid" $ snd <$> getItemId (item i)
	, ("extension", sanitizeFilePath $ takeExtension $ location i)
	]
  where
	field k v = 
		let s = sanitizeFilePath v in
		if null s then (k, "none") else (k, s)
	fieldMaybe k Nothing = (k, "none")
	fieldMaybe k (Just v) = field k v

{- Called when there is a problem with a feed.
 - Throws an error if the feed is broken, otherwise shows a warning. -}
feedProblem :: URLString -> String -> Annex ()
feedProblem url message = ifM (checkFeedBroken url)
	( error $ message ++ " (having repeated problems with this feed!)"
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
feedState url = fromRepo . gitAnnexFeedState =<< fromUrl url Nothing
