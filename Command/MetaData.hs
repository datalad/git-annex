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
import Annex.WorkTree
import Messages.JSON (JSONActionItem(..))
import Types.Messages

import qualified Data.Set as S
import qualified Data.Map as M
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
	, batchOption :: BatchMode
	}

data GetSet = Get MetaField | GetAll | Set [ModMeta]

optParser :: CmdParamsDesc -> Parser MetaDataOptions
optParser desc = MetaDataOptions
	<$> cmdParams desc
	<*> ((Get <$> getopt) <|> (Set <$> some modopts) <|> pure GetAll)
	<*> optional parseKeyOptions
	<*> parseBatchOption
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
	case batchOption o of
		NoBatch -> do
			let seeker = case getSet o of
				Get _ -> withFilesInGit
				GetAll -> withFilesInGit
				Set _ -> withFilesInGitNonRecursive
					"Not recursively setting metadata. Use --force to do that."
			withKeyOptions (keyOptions o) False
				(startKeys now o)
				(seeker $ whenAnnexed $ start now o)
				(forFiles o)
		Batch -> withMessageState $ \s -> case outputType s of
			JSONOutput -> batchInput parseJSONInput $
				commandAction . startBatch now
			_ -> error "--batch is currently only supported in --json mode"

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

parseJSONInput :: String -> Either String (Either FilePath Key, MetaData)
parseJSONInput i = do
	v <- eitherDecode (BU.fromString i)
	let m = case itemAdded v of
		Nothing -> emptyMetaData
		Just (MetaDataFields m') -> m'
	case (itemKey v, itemFile v) of
		(Just k, _) -> Right (Right k, m)
		(Nothing, Just f) -> Right (Left f, m)
		(Nothing, Nothing) -> Left "JSON input is missing either file or key"

startBatch :: POSIXTime -> (Either FilePath Key, MetaData) -> CommandStart
startBatch now (i, (MetaData m)) = case i of
	Left f -> do
		mk <- lookupFile f
		case mk of
			Just k -> go k (mkActionItem (Just f))
			Nothing -> error $ "not an annexed file: " ++ f
	Right k -> go k (mkActionItem k)
  where
	go k ai = do
		showStart' "metadata" k ai
		let o = MetaDataOptions
			{ forFiles = []
			, getSet = if MetaData m == emptyMetaData
				then GetAll
				else Set $ map mkModMeta (M.toList m)
			, keyOptions = Nothing
			, batchOption = NoBatch
			}
		next $ perform now o k
	mkModMeta (f, s)
		| S.null s = DelMeta f Nothing
		| otherwise = SetMeta f s
