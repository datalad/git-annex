{- git-annex options
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitAnnex.Options where

import System.Console.GetOpt

import Common.Annex
import qualified Git.Config
import Git.Types
import Command
import Types.TrustLevel
import Types.NumCopies
import Types.Messages
import qualified Annex
import qualified Remote
import qualified Limit
import qualified Limit.Wanted
import qualified Option

options :: [Option]
options = Option.common ++
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
	, Option ['x'] ["exclude"] (ReqArg Limit.addExclude paramGlob)
		"skip files matching the glob pattern"
	, Option ['I'] ["include"] (ReqArg Limit.addInclude paramGlob)
		"limit to files matching the glob pattern"
	, Option ['i'] ["in"] (ReqArg Limit.addIn paramRemote)
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
	, Option [] ["largerthan"] (ReqArg Limit.addLargerThan paramSize)
		"match files larger than a size"
	, Option [] ["smallerthan"] (ReqArg Limit.addSmallerThan paramSize)
		"match files smaller than a size"
	, Option [] ["want-get"] (NoArg Limit.Wanted.addWantGet)
		"match files the repository wants to get"
	, Option [] ["want-drop"] (NoArg Limit.Wanted.addWantDrop)
		"match files the repository wants to drop"
	, Option ['T'] ["time-limit"] (ReqArg Limit.addTimeLimit paramTime)
		"stop after the specified amount of time"
	, Option [] ["user-agent"] (ReqArg setuseragent paramName)
		"override default User-Agent"
	, Option [] ["trust-glacier"] (NoArg (Annex.setFlag "trustglacier"))
		"Trust Amazon Glacier inventory"
	] ++ Option.matcher
  where
	trustArg t = ReqArg (Remote.forceTrust t) paramRemote
	setnumcopies v = maybe noop
		(\n -> Annex.changeState $ \s -> s { Annex.forcenumcopies = Just $ NumCopies n })
		(readish v)
	setuseragent v = Annex.changeState $ \s -> s { Annex.useragent = Just v }
	setgitconfig v = inRepo (Git.Config.store v)
		>>= pure . (\r -> r { gitGlobalOpts = gitGlobalOpts r ++ [Param "-c", Param v] })
		>>= Annex.changeGitRepo

keyOptions :: [Option]
keyOptions = 
	[ Option ['A'] ["all"] (NoArg (Annex.setFlag "all"))
		"operate on all versions of all files"
	, Option ['U'] ["unused"] (NoArg (Annex.setFlag "unused"))
		"operate on files found by last run of git-annex unused"
	]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "source remote"

toOption :: Option
toOption = Option.field ['t'] "to" paramRemote "destination remote"

fromToOptions :: [Option]
fromToOptions = [fromOption, toOption]

jsonOption :: Option
jsonOption = Option ['j'] ["json"] (NoArg (Annex.setOutput JSONOutput))
	"enable JSON output"
