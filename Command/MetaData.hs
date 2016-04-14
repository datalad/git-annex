{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.MetaData where

import Command
import Annex.MetaData
import Logs.MetaData

import qualified Data.Set as S
import Data.Time.Clock.POSIX

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
start now o file = start' (Just file) now o

startKeys :: POSIXTime -> MetaDataOptions -> Key -> CommandStart
startKeys = start' Nothing

start' :: AssociatedFile -> POSIXTime -> MetaDataOptions -> Key -> CommandStart
start' afile now o k = case getSet o of
	Get f -> do
		l <- S.toList . currentMetaDataValues f <$> getCurrentMetaData k
		liftIO $ forM_ l $
			putStrLn . fromMetaValue
		stop
	_ -> do
		showStart' "metadata" k afile
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
	l <- map unwrapmeta . fromMetaData <$> getCurrentMetaData k
	maybeShowJSON l
	showLongNote $ unlines $ concatMap showmeta l
	return True
  where
	unwrapmeta (f, v) = (fromMetaField f, map fromMetaValue (S.toList v))
	showmeta (f, vs) = map ((f ++ "=") ++) vs
