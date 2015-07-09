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
import qualified Annex
import qualified Remote
import qualified Limit
import qualified Limit.Wanted
import CmdLine.Option
import CmdLine.Usage

-- Options that are accepted by all git-annex sub-commands,
-- although not always used.
gitAnnexOptions :: [Option]
gitAnnexOptions = commonOptions ++
	[ Option ['N'] ["numcopies"] (ReqArg setnumcopies paramNumber)
		"override default number of copies"
	, Option [] ["trust"] (trustArg Trusted)
		"override trust setting"
	, Option [] ["semitrust"] (trustArg SemiTrusted)
		"override trust setting back to default"
	, Option [] ["untrust"] (trustArg UnTrusted)
		"override trust setting to untrusted"
	, Option ['c'] ["config"] (ReqArg setgitconfig "NAME=VALUE")
		"override git configuration setting"
	, Option [] ["user-agent"] (ReqArg setuseragent paramName)
		"override default User-Agent"
	, Option [] ["trust-glacier"] (NoArg (Annex.setFlag "trustglacier"))
		"Trust Amazon Glacier inventory"
	]
  where
	trustArg t = ReqArg (Remote.forceTrust t) paramRemote
	setnumcopies v = maybe noop
		(\n -> Annex.changeState $ \s -> s { Annex.forcenumcopies = Just $ NumCopies n })
		(readish v)
	setuseragent v = Annex.changeState $ \s -> s { Annex.useragent = Just v }
	setgitconfig v = inRepo (Git.Config.store v)
		>>= pure . (\r -> r { gitGlobalOpts = gitGlobalOpts r ++ [Param "-c", Param v] })
		>>= Annex.changeGitRepo

-- Options for acting on keys, rather than work tree files.
data KeyOptions = KeyOptions
	{ wantAllKeys :: Bool
	, wantUnusedKeys :: Bool
	, wantIncompleteKeys :: Bool
	, wantSpecificKey :: Maybe Key
	}

parseKeyOptions :: Bool -> Parser KeyOptions
parseKeyOptions allowincomplete = KeyOptions
	<$> parseAllKeysOption
	<*> parseUnusedKeysOption
	<*> (if allowincomplete then parseIncompleteOption else pure False)
	<*> parseSpecificKeyOption

parseAllKeysOption :: Parser Bool
parseAllKeysOption = switch
	( long "all" <> short 'A'
	<> help "operate on all versions of all files"
	)

parseUnusedKeysOption :: Parser Bool
parseUnusedKeysOption = switch
	( long "unused" <> short 'U'
	<> help "operate on files found by last run of git-annex unused"
	)

parseSpecificKeyOption :: Parser (Maybe Key)
parseSpecificKeyOption = optional $ option (str >>= parseKey)
	( long "key" <> metavar paramKey
	<> help "operate on specified key"
	)

parseKey :: Monad m => String -> m Key
parseKey = maybe (fail "invalid key") return . file2key

parseIncompleteOption :: Parser Bool
parseIncompleteOption = switch
	( long "incomplete"
	<> help "resume previous downloads"
	)

-- Options to match properties of annexed files.
annexedMatchingOptions :: [Option]
annexedMatchingOptions = concat
	[ nonWorkTreeMatchingOptions'
	, fileMatchingOptions'
	, combiningOptions
	, [timeLimitOption]
	]

-- Matching options that don't need to examine work tree files.
nonWorkTreeMatchingOptions :: [Option]
nonWorkTreeMatchingOptions = nonWorkTreeMatchingOptions' ++ combiningOptions

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
fileMatchingOptions = fileMatchingOptions' ++ combiningOptions

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

combiningOptions :: [Option]
combiningOptions =
	[ longopt "not" "negate next option"
	, longopt "and" "both previous and next option must match"
	, longopt "or" "either previous or next option must match"
	, shortopt "(" "open group of options"
	, shortopt ")" "close group of options"
	]
  where
	longopt o = Option [] [o] $ NoArg $ Limit.addToken o
	shortopt o = Option o [] $ NoArg $ Limit.addToken o

fromOption :: Option
fromOption = fieldOption ['f'] "from" paramRemote "source remote"

toOption :: Option
toOption = fieldOption ['t'] "to" paramRemote "destination remote"

fromToOptions :: [Option]
fromToOptions = [fromOption, toOption]

jsonOption :: Option
jsonOption = Option ['j'] ["json"] (NoArg (Annex.setOutput JSONOutput))
	"enable JSON output"

jobsOption :: Option
jobsOption = Option ['J'] ["jobs"] (ReqArg set paramNumber)
	"enable concurrent jobs"
  where
	set s = case readish s of
		Nothing -> error "Bad --jobs number"
		Just n -> Annex.setOutput (ParallelOutput n)

timeLimitOption :: Option
timeLimitOption = Option ['T'] ["time-limit"]
	(ReqArg Limit.addTimeLimit paramTime)
	"stop after the specified amount of time"

autoOption :: Option
autoOption = flagOption ['a'] "auto" "automatic mode"

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
