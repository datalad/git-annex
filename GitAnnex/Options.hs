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
import Command
import Types.TrustLevel
import qualified Annex
import qualified Remote
import qualified Limit
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
		"don't skip files matching the glob pattern"
	, Option ['i'] ["in"] (ReqArg Limit.addIn paramRemote)
		"skip files not present in a remote"
	, Option ['C'] ["copies"] (ReqArg Limit.addCopies paramNumber)
		"skip files with fewer copies"
	, Option ['B'] ["inbackend"] (ReqArg Limit.addInBackend paramName)
		"skip files not using a key-value backend"
	, Option [] ["inallgroup"] (ReqArg Limit.addInAllGroup paramGroup)
		"skip files not present in all remotes in a group"
	, Option [] ["largerthan"] (ReqArg Limit.addLargerThan paramSize)
		"skip files larger than a size"
	, Option [] ["smallerthan"] (ReqArg Limit.addSmallerThan paramSize)
		"skip files smaller than a size"
	, Option ['T'] ["time-limit"] (ReqArg Limit.addTimeLimit paramTime)
		"stop after the specified amount of time"
	, Option [] ["trust-glacier"] (NoArg (Annex.setFlag "trustglacier"))
		"Trust Amazon Glacier inventory"
	] ++ Option.matcher
  where
	setnumcopies v = maybe noop
		(\n -> Annex.changeGitConfig $ \c -> c { annexNumCopies = n })
		(readish v)
	setgitconfig v = Annex.changeGitRepo =<< inRepo (Git.Config.store v)

	trustArg t = ReqArg (Remote.forceTrust t) paramRemote

keyOptions :: [Option]
keyOptions = 
	[ Option ['A'] ["all"] (NoArg (Annex.setFlag "all"))
		"operate on all versions of all files"
	, Option ['U'] ["unused"] (NoArg (Annex.setFlag "unused"))
		"operate on files found by last run of git-annex unused"
	]
