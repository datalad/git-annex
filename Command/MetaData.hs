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
import Types.Messages
import Utility.SafeOutput
import Limit
import Messages.JSON (JSONActionItem(..), eitherDecode)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8 as BU
import Control.Concurrent

cmd :: Command
cmd = withAnnexOptions [jsonOptions, annexedMatchingOptions] $ 
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
	<*> parseBatchOption False
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
		c <- currentVectorClock
		let ww = WarnUnmatchLsFiles "metadata"
		let seeker = AnnexedFileSeeker
			{ startAction = const $ start c o
			, checkContentPresent = Nothing
			, usesLocationLog = False
			}
		let seekaction = case getSet o of
			Get _ -> withFilesInGitAnnex ww
			GetAll -> withFilesInGitAnnex ww
			Set _ -> withFilesInGitAnnexNonRecursive ww
				"Not recursively setting metadata. Use --force to do that."
		withKeyOptions (keyOptions o) False seeker
			(commandAction . startKeys c o)
			(seekaction seeker)
			=<< workTreeItems ww (forFiles o)
	Batch fmt -> withMessageState $ \s -> case outputType s of
		JSONOutput _ -> ifM limited
			( giveup "combining --batch with file matching options is not currently supported"
			, batchOnly (keyOptions o) (forFiles o) $
				batchInput fmt parseJSONInput 
					(commandAction . batchCommandStart . startBatch)
			)
		_ -> giveup "--batch is currently only supported in --json mode"

start :: CandidateVectorClock -> MetaDataOptions -> SeekInput -> OsPath -> Key -> CommandStart
start c o si file k = startKeys c o (si, k, mkActionItem (k, afile))
  where
	afile = AssociatedFile (Just file)

startKeys :: CandidateVectorClock -> MetaDataOptions -> (SeekInput, Key, ActionItem) -> CommandStart
startKeys c o (si, k, ai) = case getSet o of
	Get f -> startingCustomOutput k $ do
		l <- S.toList . currentMetaDataValues f <$> getCurrentMetaData k
		liftIO $ forM_ l $
			B8.putStrLn . safeOutput . fromMetaValue
		next $ return True
	_ -> starting "metadata" ai si $
		perform c o k

perform :: CandidateVectorClock -> MetaDataOptions -> Key -> CommandPerform
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
	maybeAddJSONField "fields" m
	showLongNote $ UnquotedString $ unlines $ concatMap showmeta $
		map unwrapmeta (fromMetaData m)
	return True
  where
	unwrapmeta (f, v) = (fromMetaField f, map fromMetaValue (S.toList v))
	showmeta (f, vs) = map ((T.unpack f ++ "=") ++) (map decodeBS vs)

parseJSONInput :: String -> Annex (Either String (Either OsPath Key, MetaData))
parseJSONInput i = case eitherDecode (BU.fromString i) of
	Left e -> return (Left e)
	Right v -> do
		let m = case itemFields v of
			Nothing -> emptyMetaData
			Just m' -> m'
		case (itemKey v, itemFile v) of
			(Just k, _) -> return $
				Right (Right k, m)
			(Nothing, Just f) -> do
				f' <- liftIO $ relPathCwdToFile f
				return $ Right (Left f', m)
			(Nothing, Nothing) -> return $ 
				Left "JSON input is missing either file or key"

startBatch :: (SeekInput, (Either OsPath Key, MetaData)) -> CommandStart
startBatch (si, (i, (MetaData m))) = case i of
	Left f -> do
		mk <- lookupKeyStaged f
		case mk of
			Just k -> go k (mkActionItem (k, AssociatedFile (Just f)))
			Nothing -> return Nothing
	Right k -> go k (mkActionItem k)
  where
	go k ai = starting "metadata" ai si $ do
		let o = MetaDataOptions
			{ forFiles = []
			, getSet = if MetaData m == emptyMetaData
				then GetAll
				else Set $ map mkModMeta (M.toList m)
			, keyOptions = Nothing
			, batchOption = NoBatch
			}
		t <- currentVectorClock
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
