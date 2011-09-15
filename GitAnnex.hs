{- git-annex main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitAnnex where

import System.Console.GetOpt
import Control.Monad.State (liftIO)

import qualified Git
import CmdLine
import Command
import Options
import Utility
import Types
import Types.TrustLevel
import qualified Annex
import qualified Remote

import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.SetKey
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Describe
import qualified Command.InitRemote
import qualified Command.Fsck
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.Whereis
import qualified Command.Merge
import qualified Command.Status
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.AddUrl
import qualified Command.Map
import qualified Command.Upgrade
import qualified Command.Version

cmds :: [Command]
cmds = concat
	[ Command.Add.command
	, Command.Get.command
	, Command.Drop.command
	, Command.Move.command
	, Command.Copy.command
	, Command.Unlock.command
	, Command.Lock.command
	, Command.Init.command
	, Command.Describe.command
	, Command.InitRemote.command
	, Command.Unannex.command
	, Command.Uninit.command
	, Command.PreCommit.command
	, Command.Trust.command
	, Command.Untrust.command
	, Command.Semitrust.command
	, Command.AddUrl.command
	, Command.FromKey.command
	, Command.DropKey.command
	, Command.SetKey.command
	, Command.Fix.command
	, Command.Fsck.command
	, Command.Unused.command
	, Command.DropUnused.command
	, Command.Find.command
	, Command.Whereis.command
	, Command.Merge.command
	, Command.Status.command
	, Command.Migrate.command
	, Command.Map.command
	, Command.Upgrade.command
	, Command.Version.command
	]

options :: [Option]
options = commonOptions ++
	[ Option ['k'] ["key"] (ReqArg setkey paramKey)
		"specify a key to use"
	, Option ['t'] ["to"] (ReqArg setto paramRemote)
		"specify to where to transfer content"
	, Option ['f'] ["from"] (ReqArg setfrom paramRemote)
		"specify from where to transfer content"
	, Option ['x'] ["exclude"] (ReqArg addexclude paramGlob)
		"skip files matching the glob pattern"
	, Option ['N'] ["numcopies"] (ReqArg setnumcopies paramNumber)
		"override default number of copies"
	, Option [] ["trust"] (ReqArg (Remote.forceTrust Trusted) paramRemote)
		"override trust setting"
	, Option [] ["semitrust"] (ReqArg (Remote.forceTrust SemiTrusted) paramRemote)
		"override trust setting back to default"
	, Option [] ["untrust"] (ReqArg (Remote.forceTrust UnTrusted) paramRemote)
		"override trust setting to untrusted"
	, Option ['c'] ["config"] (ReqArg setgitconfig "NAME=VALUE")
		"override git configuration setting"
	]
	where
		setto v = Annex.changeState $ \s -> s { Annex.toremote = Just v }
		setfrom v = Annex.changeState $ \s -> s { Annex.fromremote = Just v }
		addexclude v = Annex.changeState $ \s -> s { Annex.exclude = v:Annex.exclude s }
		setnumcopies v = Annex.changeState $ \s -> s {Annex.forcenumcopies = readMaybe v }
		setkey v = Annex.changeState $ \s -> s { Annex.defaultkey = Just v }
		setgitconfig :: String -> Annex ()
		setgitconfig v = do
			g <- Annex.gitRepo
			g' <- liftIO $ Git.configStore g v
			Annex.changeState $ \s -> s { Annex.repo = g' }

header :: String
header = "Usage: git-annex command [option ..]"

run :: [String] -> IO ()
run args = dispatch args cmds options header =<< Git.repoFromCwd
