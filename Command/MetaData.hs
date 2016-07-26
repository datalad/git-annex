{- git-annex command
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.MetaData where

import Command
import Annex.MetaData
import Logs.MetaData
import Messages.JSON (ParsedJSON(..))

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.UTF8 as BU
import Data.Time.Clock.POSIX
import Data.Aeson

cmd :: Command
cmd = withGlobalOptions ([jsonOption] ++ annexedMatchingOptions) $ 
	command "metadata" SectionMetaData
		"sets or gets metadata of a file"
		paramPaths (seek <$$> optParser)

data MetaDataOptions = MetaDataOptions
	{ forFiles :: CmdParams
	, getSet :: GetSet
	, keyOptions :: Maybe KeyOptions
	}

data GetSet = Get MetaField | GetAll | Set [ModMeta]

optParser :: CmdParamsDesc -> Parser MetaDataOptions
optParser desc = MetaDataOptions
	<$> cmdParams desc
	<*> ((Get <$> getopt) <|> (Set <$> some modopts) <|> pure GetAll)
	<*> optional (parseKeyOptions False)
  where
	getopt = option (eitherReader mkMetaField)
		( long "get" <> short 'g' <> metavar paramField
		<> help "get single metadata field"
		)
	modopts = option (eitherReader parseModMeta)
		( long "set" <> short 's' <> metavar "FIELD[+-]=VALUE"
		<> help "set or unset metadata value"
		)
		<|> (AddMeta tagMetaField . toMetaValue <$> strOption
			( long "tag" <> short 't' <> metavar "TAG"
			<> help "set a tag"
			))
		<|> (DelMeta tagMetaField . Just . toMetaValue <$> strOption
			( long "untag" <> short 'u' <> metavar "TAG"
			<> help "remove a tag"
			))
		<|> option (eitherReader (\f -> DelMeta <$> mkMetaField f <*> pure Nothing))
			( long "remove"  <> short 'r' <> metavar "FIELD"
			<> help "remove all values of a field"
			)

seek :: MetaDataOptions -> CommandSeek
seek o = do
	now <- liftIO getPOSIXTime
	let seeker = case getSet o of
		Get _ -> withFilesInGit
		GetAll -> withFilesInGit
		Set _ -> withFilesInGitNonRecursive
			"Not recursively setting metadata. Use --force to do that."
	withKeyOptions (keyOptions o) False
		(startKeys now o)
		(seeker $ whenAnnexed $ start now o)
		(forFiles o)

start :: POSIXTime -> MetaDataOptions -> FilePath -> Key -> CommandStart
start now o file k = startKeys now o k (mkActionItem afile)
  where
	afile = Just file

startKeys :: POSIXTime -> MetaDataOptions -> Key -> ActionItem -> CommandStart
startKeys now o k ai = case getSet o of
	Get f -> do
		l <- S.toList . currentMetaDataValues f <$> getCurrentMetaData k
		liftIO $ forM_ l $
			putStrLn . fromMetaValue
		stop
	_ -> do
		showStart' "metadata" k ai
		next $ perform now o k

perform :: POSIXTime -> MetaDataOptions -> Key -> CommandPerform
perform now o k = case getSet o of
	Set ms -> do
		oldm <- getCurrentMetaData k
		let m = combineMetaData $ map (modMeta oldm) ms
		addMetaData' k m now
		next $ cleanup k
	_ -> next $ cleanup k

cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	let Object o = toJSON (MetaDataFields m)
	maybeShowJSON $ AesonObject o
	showLongNote $ unlines $ concatMap showmeta $
		map unwrapmeta (fromMetaData m)
	return True
  where
	unwrapmeta (f, v) = (fromMetaField f, map fromMetaValue (S.toList v))
	showmeta (f, vs) = map ((f ++ "=") ++) vs

-- Metadata serialized to JSON in the field named "fields" of
-- a larger object.
newtype MetaDataFields = MetaDataFields MetaData
	deriving (Show)

instance ToJSON MetaDataFields where
	toJSON (MetaDataFields m) = object [ (fieldsField, toJSON m) ]

instance FromJSON MetaDataFields where
	parseJSON (Object v) = do
		f <- v .: fieldsField
		case f of
			Nothing -> return (MetaDataFields emptyMetaData)
			Just v' -> MetaDataFields <$> parseJSON v'
	parseJSON _ = fail "expected an object"

fieldsField :: T.Text
fieldsField = T.pack "fields"

parseJSONInput :: String -> Maybe (Either FilePath Key, MetaData)
parseJSONInput i = do
	v <- decode (BU.fromString i)
	case parsedAdded v of
		Nothing -> return (parsedKeyfile v, emptyMetaData)
		Just (MetaDataFields m) -> return (parsedKeyfile v, m)
