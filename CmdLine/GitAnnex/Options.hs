{- git-annex command-line option parsing
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitAnnex.Options where

import System.Console.GetOpt
import Options.Applicative

import Common.Annex
import qualified Git.Config
import Git.Types
import Types.TrustLevel
import Types.NumCopies
import Types.Messages
import Types.Key
import Types.Command
import Types.DeferredParse
import Types.DesktopNotify
import qualified Annex
import qualified Remote
import qualified Limit
import qualified Limit.Wanted
import CmdLine.Option
import CmdLine.Usage

-- Global options that are accepted by all git-annex sub-commands,
-- although not always used.
gitAnnexGlobalOptions :: [Parser GlobalSetter]
gitAnnexGlobalOptions = commonGlobalOptions ++
	[ globalSetter setnumcopies $ option auto
		( long "numcopies" <> short 'N' <> metavar paramNumber
		<> help "override default number of copies"
		)
	, globalSetter (Remote.forceTrust Trusted) $ strOption
		( long "trust" <> metavar paramRemote
		<> help "override trust setting"
		)
	, globalSetter (Remote.forceTrust SemiTrusted) $ strOption
		( long "semitrust" <> metavar paramRemote
		<> help "override trust setting back to default"
		)
	, globalSetter (Remote.forceTrust UnTrusted) $ strOption
		( long "untrust" <> metavar paramRemote
		<> help "override trust setting to untrusted"
		)
	, globalSetter setgitconfig $ strOption
		( long "config" <> short 'c' <> metavar "NAME=VALUE"
		<> help "override git configuration setting"
		)
	, globalSetter setuseragent $ strOption
		( long "user-agent" <> metavar paramName
		<> help "override default User-Agent"
		)
	, globalFlag (Annex.setFlag "trustglacier")
		( long "trust-glacier"
		<> help "Trust Amazon Glacier inventory"
		)
	, globalFlag (setdesktopnotify mkNotifyFinish)
		( long "notify-finish"
		<> help "show desktop notification after transfer finishes"
		)
	, globalFlag (setdesktopnotify mkNotifyStart)
		( long "notify-start"
		<> help "show desktop notification after transfer completes"
		)
	]
  where
	setnumcopies n = Annex.changeState $ \s -> s { Annex.forcenumcopies = Just $ NumCopies n }
	setuseragent v = Annex.changeState $ \s -> s { Annex.useragent = Just v }
	setgitconfig v = inRepo (Git.Config.store v)
		>>= pure . (\r -> r { gitGlobalOpts = gitGlobalOpts r ++ [Param "-c", Param v] })
		>>= Annex.changeGitRepo
	setdesktopnotify v = Annex.changeState $ \s -> s { Annex.desktopnotify = Annex.desktopnotify s <> v }

parseRemoteOption :: Parser RemoteName -> Parser (DeferredParse Remote)
parseRemoteOption p = DeferredParse . (fromJust <$$> Remote.byNameWithUUID) . Just <$> p

data FromToOptions
	= FromRemote (DeferredParse Remote)
	| ToRemote (DeferredParse Remote)

instance DeferredParseClass FromToOptions where
	finishParse (FromRemote v) = FromRemote <$> finishParse v
	finishParse (ToRemote v) = ToRemote <$> finishParse v

parseFromToOptions :: Parser FromToOptions
parseFromToOptions = 
	(FromRemote <$> parseFromOption) 
	<|> (ToRemote <$> parseToOption)

parseFromOption :: Parser (DeferredParse Remote)
parseFromOption = parseRemoteOption $ strOption
	( long "from" <> short 'f' <> metavar paramRemote
	<> help "source remote"
	)

parseToOption :: Parser (DeferredParse Remote)
parseToOption = parseRemoteOption $ strOption
	( long "to" <> short 't' <> metavar paramRemote
	<> help "destination remote"
	)

-- Options for acting on keys, rather than work tree files.
data KeyOptions
	= WantAllKeys
	| WantUnusedKeys
	| WantSpecificKey Key
	| WantIncompleteKeys

parseKeyOptions :: Bool -> Parser KeyOptions
parseKeyOptions allowincomplete = if allowincomplete
	then base
		<|> flag' WantIncompleteKeys
			( long "incomplete"
			<> help "resume previous downloads"
			)
	else base
  where
	base = parseAllOption
		<|> flag' WantUnusedKeys
			( long "unused" <> short 'U'
			<> help "operate on files found by last run of git-annex unused"
			)
		<|> (WantSpecificKey <$> option (str >>= parseKey)
			( long "key" <> metavar paramKey
			<> help "operate on specified key"
			))

parseAllOption :: Parser KeyOptions
parseAllOption = flag' WantAllKeys
	( long "all" <> short 'A'
	<> help "operate on all versions of all files"
	)

parseKey :: Monad m => String -> m Key
parseKey = maybe (fail "invalid key") return . file2key

-- Options to match properties of annexed files.
annexedMatchingOptions :: [Option]
annexedMatchingOptions = concat
	[ nonWorkTreeMatchingOptions'
	, fileMatchingOptions'
	-- , combiningOptions
	-- , [timeLimitOption]
	]

-- Matching options that don't need to examine work tree files.
nonWorkTreeMatchingOptions :: [Option]
nonWorkTreeMatchingOptions = nonWorkTreeMatchingOptions' -- ++ combiningOptions

nonWorkTreeMatchingOptions' :: [Option]
nonWorkTreeMatchingOptions' = 
	[ Option ['i'] ["in"] (ReqArg Limit.addIn paramRemote)
		"match files present in a remote"
	, Option ['C'] ["copies"] (ReqArg Limit.addCopies paramNumber)
		"skip files with fewer copies"
	, Option [] ["lackingcopies"] (ReqArg (Limit.addLackingCopies False) paramNumber)
		"match files that need more copies"
	, Option [] ["approxlackingcopies"] (ReqArg (Limit.addLackingCopies True) paramNumber)
		"match files that need more copies (faster)"
	, Option ['B'] ["inbackend"] (ReqArg Limit.addInBackend paramName)
		"match files using a key-value backend"
	, Option [] ["inallgroup"] (ReqArg Limit.addInAllGroup paramGroup)
		"match files present in all remotes in a group"
	, Option [] ["metadata"] (ReqArg Limit.addMetaData "FIELD=VALUE")
		"match files with attached metadata"
	, Option [] ["want-get"] (NoArg Limit.Wanted.addWantGet)
		"match files the repository wants to get"
	, Option [] ["want-drop"] (NoArg Limit.Wanted.addWantDrop)
		"match files the repository wants to drop"
	]

-- Options to match files which may not yet be annexed.
fileMatchingOptions :: [Option]
fileMatchingOptions = fileMatchingOptions' -- ++ combiningOptions

fileMatchingOptions' :: [Option]
fileMatchingOptions' =
	[ Option ['x'] ["exclude"] (ReqArg Limit.addExclude paramGlob)
		"skip files matching the glob pattern"
	, Option ['I'] ["include"] (ReqArg Limit.addInclude paramGlob)
		"limit to files matching the glob pattern"
	, Option [] ["largerthan"] (ReqArg Limit.addLargerThan paramSize)
		"match files larger than a size"
	, Option [] ["smallerthan"] (ReqArg Limit.addSmallerThan paramSize)
		"match files smaller than a size"
	]

parseCombiningOptions :: Parser [GlobalSetter]
parseCombiningOptions = 
	many $ longopt "not" "negate next option"
		<|> longopt "and" "both previous and next option must match"
		<|> longopt "or" "either previous or next option must match"
		<|> shortopt '(' "open group of options"
		<|> shortopt ')' "close group of options"
  where
	longopt o h = globalFlag (Limit.addToken o) ( long o <> help h )
	shortopt o h = globalFlag (Limit.addToken [o]) ( short o <> help h)

parseJsonOption :: Parser GlobalSetter
parseJsonOption = globalFlag (Annex.setOutput JSONOutput)
	( long "json" <> short 'j'
	<> help "enable JSON output"
	)

parseJobsOption :: Parser GlobalSetter
parseJobsOption = globalSetter (Annex.setOutput . ParallelOutput) $ 
	option auto
		( long "jobs" <> short 'J' <> metavar paramNumber
		<> help "enable concurrent jobs"
		)

parseTimeLimitOption :: Parser GlobalSetter
parseTimeLimitOption = globalSetter Limit.addTimeLimit $ strOption
	( long "time-limit" <> short 'T' <> metavar paramTime
	<> help "stop after the specified amount of time"
	)

parseAutoOption :: Parser Bool
parseAutoOption = switch
	( long "auto" <> short 'a'
	<> help "automatic mode"
	)

{- Parser that accepts all non-option params. -}
cmdParams :: CmdParamsDesc -> Parser CmdParams
cmdParams paramdesc = many $ argument str
	( metavar paramdesc
	-- Let bash completion complete files
	<> action "file"
	)
