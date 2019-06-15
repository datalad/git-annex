{- git-annex command
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.MetaData where

import Command
import Annex.MetaData
import Annex.VectorClock
import Logs.MetaData
import Annex.WorkTree
import Messages.JSON (JSONActionItem(..))
import Types.Messages
import Utility.Aeson
import Limit

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8 as BU
import Control.Concurrent

cmd :: Command
cmd = withGlobalOptions [jsonOptions, annexedMatchingOptions] $ 
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
	getopt = option (eitherReader (mkMetaField . T.pack))
		( long "get" <> short 'g' <> metavar paramField
		<> help "get single metadata field"
		)
	modopts = option (eitherReader parseModMeta)
		( long "set" <> short 's' <> metavar "FIELD[+-]=VALUE"
		<> help "set or unset metadata value"
		)
		<|> (AddMeta tagMetaField . toMetaValue . encodeBS <$> strOption
			( long "tag" <> short 't' <> metavar "TAG"
			<> help "set a tag"
			))
		<|> (DelMeta tagMetaField . Just . toMetaValue . encodeBS <$> strOption
			( long "untag" <> short 'u' <> metavar "TAG"
			<> help "remove a tag"
			))
		<|> option (eitherReader (\f -> DelMeta <$> mkMetaField (T.pack f) <*> pure Nothing))
			( long "remove"  <> short 'r' <> metavar "FIELD"
			<> help "remove all values of a field"
			)
		<|> flag' DelAllMeta
			( long "remove-all"
			<> help "remove all metadata"
			)

seek :: MetaDataOptions -> CommandSeek
seek o = case batchOption o of
	NoBatch -> do
		c <- liftIO currentVectorClock
		let seeker = case getSet o of
			Get _ -> withFilesInGit
			GetAll -> withFilesInGit
			Set _ -> withFilesInGitNonRecursive
				"Not recursively setting metadata. Use --force to do that."
		withKeyOptions (keyOptions o) False
			(commandAction . startKeys c o)
			(seeker (commandAction . (whenAnnexed (start c o))))
			=<< workTreeItems (forFiles o)
	Batch fmt -> withMessageState $ \s -> case outputType s of
		JSONOutput _ -> ifM limited
			( giveup "combining --batch with file matching options is not currently supported"
			, batchInput fmt parseJSONInput $
				commandAction . startBatch
			)
		_ -> giveup "--batch is currently only supported in --json mode"

start :: VectorClock -> MetaDataOptions -> FilePath -> Key -> CommandStart
start c o file k = startKeys c o (k, mkActionItem (k, afile))
  where
	afile = AssociatedFile (Just file)

startKeys :: VectorClock -> MetaDataOptions -> (Key, ActionItem) -> CommandStart
startKeys c o (k, ai) = case getSet o of
	Get f -> startingCustomOutput k $ do
		l <- S.toList . currentMetaDataValues f <$> getCurrentMetaData k
		liftIO $ forM_ l $
			B8.putStrLn . fromMetaValue
		next $ return True
	_ -> starting "metadata" ai $
		perform c o k

perform :: VectorClock -> MetaDataOptions -> Key -> CommandPerform
perform c o k = case getSet o of
	Set ms -> do
		oldm <- getCurrentMetaData k
		let m = combineMetaData $ map (modMeta oldm) ms
		addMetaDataClocked k m c
		next $ cleanup k
	_ -> next $ cleanup k

cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	let Object o = toJSON' (MetaDataFields m)
	maybeShowJSON $ AesonObject o
	showLongNote $ unlines $ concatMap showmeta $
		map unwrapmeta (fromMetaData m)
	return True
  where
	unwrapmeta (f, v) = (fromMetaField f, map fromMetaValue (S.toList v))
	showmeta (f, vs) = map ((T.unpack f ++ "=") ++) (map decodeBS vs)

-- Metadata serialized to JSON in the field named "fields" of
-- a larger object.
newtype MetaDataFields = MetaDataFields MetaData
	deriving (Show)

instance ToJSON' MetaDataFields where
	toJSON' (MetaDataFields m) = object [ (fieldsField, toJSON' m) ]

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

startBatch :: (Either FilePath Key, MetaData) -> CommandStart
startBatch (i, (MetaData m)) = case i of
	Left f -> do
		mk <- lookupFile f
		case mk of
			Just k -> go k (mkActionItem (k, AssociatedFile (Just f)))
			Nothing -> giveup $ "not an annexed file: " ++ f
	Right k -> go k (mkActionItem k)
  where
	go k ai = starting "metadata" ai $ do
		let o = MetaDataOptions
			{ forFiles = []
			, getSet = if MetaData m == emptyMetaData
				then GetAll
				else Set $ map mkModMeta (M.toList m)
			, keyOptions = Nothing
			, batchOption = NoBatch
			}
		t <- liftIO currentVectorClock
		-- It would be bad if two batch mode changes used exactly
		-- the same timestamp, since the order of adds and removals
		-- of the same metadata value would then be indeterminate.
		-- To guarantee that never happens, delay 1 microsecond,
		-- so the timestamp will always be different. This is
		-- probably less expensive than cleaner methods,
		-- such as taking from a list of increasing timestamps.
		liftIO $ threadDelay 1
		perform t o k
	mkModMeta (f, s)
		| S.null s = DelMeta f Nothing
		| otherwise = SetMeta f s
