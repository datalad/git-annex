{- git-annex main program
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP, OverloadedStrings #-}

module GitAnnex where

import qualified Git.CurrentRepo
import CmdLine
import Command
import GitAnnex.Options

import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.FromKey
import qualified Command.DropKey
import qualified Command.TransferKey
import qualified Command.TransferKeys
import qualified Command.ReKey
import qualified Command.Reinject
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Describe
import qualified Command.InitRemote
import qualified Command.EnableRemote
import qualified Command.Fsck
import qualified Command.Repair
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.AddUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.Find
import qualified Command.Whereis
import qualified Command.List
import qualified Command.Log
import qualified Command.Merge
import qualified Command.Info
import qualified Command.Status
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.Dead
import qualified Command.Group
import qualified Command.Wanted
import qualified Command.Schedule
import qualified Command.Ungroup
import qualified Command.Vicfg
import qualified Command.Sync
import qualified Command.Mirror
import qualified Command.AddUrl
#ifdef WITH_FEED
import qualified Command.ImportFeed
#endif
import qualified Command.RmUrl
import qualified Command.Import
import qualified Command.Map
import qualified Command.Direct
import qualified Command.Indirect
import qualified Command.Upgrade
import qualified Command.Forget
import qualified Command.Version
import qualified Command.Help
#ifdef WITH_ASSISTANT
import qualified Command.Watch
import qualified Command.Assistant
#ifdef WITH_WEBAPP
import qualified Command.WebApp
#endif
#ifdef WITH_XMPP
import qualified Command.XMPPGit
#endif
#endif
#ifdef WITH_TESTSUITE
import qualified Command.Test
import qualified Command.FuzzTest
#endif
#ifdef WITH_EKG
import System.Remote.Monitoring
#endif

cmds :: [Command]
cmds = concat
	[ Command.Add.def
	, Command.Get.def
	, Command.Drop.def
	, Command.Move.def
	, Command.Copy.def
	, Command.Unlock.def
	, Command.Lock.def
	, Command.Sync.def
	, Command.Mirror.def
	, Command.AddUrl.def
#ifdef WITH_FEED
	, Command.ImportFeed.def
#endif
	, Command.RmUrl.def
	, Command.Import.def
	, Command.Init.def
	, Command.Describe.def
	, Command.InitRemote.def
	, Command.EnableRemote.def
	, Command.Reinject.def
	, Command.Unannex.def
	, Command.Uninit.def
	, Command.PreCommit.def
	, Command.Trust.def
	, Command.Untrust.def
	, Command.Semitrust.def
	, Command.Dead.def
	, Command.Group.def
	, Command.Wanted.def
	, Command.Schedule.def
	, Command.Ungroup.def
	, Command.Vicfg.def
	, Command.FromKey.def
	, Command.DropKey.def
	, Command.TransferKey.def
	, Command.TransferKeys.def
	, Command.ReKey.def
	, Command.Fix.def
	, Command.Fsck.def
	, Command.Repair.def
	, Command.Unused.def
	, Command.DropUnused.def
	, Command.AddUnused.def
	, Command.Find.def
	, Command.Whereis.def
	, Command.List.def
	, Command.Log.def
	, Command.Merge.def
	, Command.Info.def
	, Command.Status.def
	, Command.Migrate.def
	, Command.Map.def
	, Command.Direct.def
	, Command.Indirect.def
	, Command.Upgrade.def
	, Command.Forget.def
	, Command.Version.def
	, Command.Help.def
#ifdef WITH_ASSISTANT
	, Command.Watch.def
	, Command.Assistant.def
#ifdef WITH_WEBAPP
	, Command.WebApp.def
#endif
#ifdef WITH_XMPP
	, Command.XMPPGit.def
#endif
#endif
#ifdef WITH_TESTSUITE
	, Command.Test.def
	, Command.FuzzTest.def
#endif
	]

header :: String
header = "git-annex command [option ...]"

run :: [String] -> IO ()
run args = do
#ifdef WITH_EKG
	_ <- forkServer "localhost" 4242
#endif
	dispatch True args cmds options [] header Git.CurrentRepo.get
