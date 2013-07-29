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
import Data.Char

import Common.Annex
import Command
import qualified Utility.Url as Url
import Logs.Web
import qualified Option
import qualified Utility.Format
import Utility.Tmp
import Command.AddUrl (addUrlFile, relaxedOption)

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
			mapM_ (downloadEnclosure relaxed cache) l
			next $ return True
		_ -> stop

data ToDownload = ToDownload
	{ feed :: Feed
	, item :: Item
	, location :: URLString
	}

mkToDownload :: Feed -> Item -> Maybe ToDownload
mkToDownload f i = case getItemEnclosure i of
	Nothing -> Nothing
	Just (enclosureurl, _, _) -> Just $ ToDownload f i enclosureurl

data Cache = Cache
	{ knownurls :: S.Set URLString
	, template :: Utility.Format.Format
	}

getCache :: Maybe String -> Annex Cache
getCache opttemplate = do
	showSideAction "checking known urls"
	us <- S.fromList <$> knownUrls
	return $ Cache us tmpl
  where
	tmpl = Utility.Format.gen $ fromMaybe defaultTemplate opttemplate

findEnclosures :: URLString -> Annex (Maybe [ToDownload])
findEnclosures url = go =<< downloadFeed url
  where
	go Nothing = do
		warning $ "failed to parse feed " ++ url
		return Nothing
	go (Just f) = return $ Just $
		mapMaybe (mkToDownload f) (feedItems f)

{- Feeds change, so a feed download cannot be resumed. -}
downloadFeed :: URLString -> Annex (Maybe Feed)
downloadFeed url = do
	showOutput
	liftIO $ withTmpFile "feed" $ \f h -> do
		fileEncoding h
		ifM (Url.download url [] [] f)
			( parseFeedString <$> hGetContentsStrict h
			, return Nothing
			)

{- Avoids downloading any urls that are already known to be associated
 - with a file in the annex. -}
downloadEnclosure :: Bool -> Cache -> ToDownload -> Annex ()
downloadEnclosure relaxed cache enclosure
	| S.member url (knownurls cache) = noop
	| otherwise = do
		dest <- liftIO $ feedFile (template cache) enclosure
		showStart "addurl" dest
		ifM (addUrlFile relaxed url dest)
			( showEndOk
			, showEndFail
			)
  where
  	url = location enclosure
	
defaultTemplate :: String
defaultTemplate = "${feedtitle}/${itemtitle}${extension}"

{- Generate a unique filename for the feed item by filling 
 - out the template.
 -
 - Since each feed url is only downloaded once,
 - if the file already exists, two items with different urls
 - has the same title. A number is added to disambiguate.
 -}
feedFile :: Utility.Format.Format -> ToDownload -> IO FilePath
feedFile tmpl i = makeUnique 1 $
	Utility.Format.format tmpl $ M.fromList
		[ field "feedtitle" $ getFeedTitle $ feed i
		, fieldMaybe "itemtitle" $ getItemTitle $ item i
		, fieldMaybe "feedauthor" $ getFeedAuthor $ feed i
		, fieldMaybe "itemauthor" $ getItemAuthor $ item i
		, fieldMaybe "itemsummary" $ getItemSummary $ item i
		, fieldMaybe "itemdescription" $ getItemDescription $ item i
		, fieldMaybe "itemrights" $ getItemRights $ item i
		, fieldMaybe "itemid" $ snd <$> getItemId (item i)
		, ("extension", map sanitize $ takeExtension $ location i)
		]
  where
	field k v = 
		let s = map sanitize v in
		if null s then (k, "none") else (k, s)
	fieldMaybe k Nothing = (k, "none")
	fieldMaybe k (Just v) = field k v

	sanitize c
		| isSpace c || isPunctuation c || c == '/' = '_'
		| otherwise = c

makeUnique :: Integer -> FilePath -> IO FilePath
makeUnique n file =
	ifM (isJust <$> catchMaybeIO (getSymbolicLinkStatus f))
		( makeUnique (n + 1) file
		, return f
		)
  where
  	f = if n < 2
		then file
		else
			let (d, base) = splitFileName file
			in d </> show n ++ "_" ++ base
