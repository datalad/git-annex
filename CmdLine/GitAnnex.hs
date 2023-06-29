{- git-annex main program
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, OverloadedStrings #-}

module CmdLine.GitAnnex where

import qualified Git.CurrentRepo
import CmdLine
import Command
import Utility.Env
import Annex.Ssh
import Annex.Multicast
import Types.Test
import Types.Benchmark

import qualified Command.Help
import qualified Command.Add
import qualified Command.Unannex
import qualified Command.Drop
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Get
import qualified Command.Fsck
import qualified Command.LookupKey
import qualified Command.CalcKey
import qualified Command.ContentLocation
import qualified Command.ExamineKey
import qualified Command.MatchExpression
import qualified Command.FromKey
import qualified Command.RegisterUrl
import qualified Command.UnregisterUrl
import qualified Command.SetKey
import qualified Command.DropKey
import qualified Command.Transferrer
import qualified Command.TransferKey
import qualified Command.TransferKeys
import qualified Command.SetPresentKey
import qualified Command.ReadPresentKey
import qualified Command.CheckPresentKey
import qualified Command.ReKey
import qualified Command.Adjust
import qualified Command.MetaData
import qualified Command.View
import qualified Command.VAdd
import qualified Command.VFilter
import qualified Command.VPop
import qualified Command.VCycle
import qualified Command.Reinject
import qualified Command.Fix
import qualified Command.Init
import qualified Command.Describe
import qualified Command.InitRemote
import qualified Command.EnableRemote
import qualified Command.ConfigRemote
import qualified Command.RenameRemote
import qualified Command.EnableTor
import qualified Command.Multicast
import qualified Command.Expire
import qualified Command.Repair
import qualified Command.Unused
import qualified Command.DropUnused
import qualified Command.AddUnused
import qualified Command.Unlock
import qualified Command.Lock
import qualified Command.PreCommit
import qualified Command.PostReceive
import qualified Command.FilterBranch
import qualified Command.Find
import qualified Command.FindKeys
import qualified Command.FindRef
import qualified Command.Whereis
import qualified Command.WhereUsed
import qualified Command.List
import qualified Command.Log
import qualified Command.Merge
import qualified Command.ResolveMerge
import qualified Command.Info
import qualified Command.Status
import qualified Command.Inprogress
import qualified Command.Migrate
import qualified Command.Uninit
import qualified Command.Reinit
import qualified Command.NumCopies
import qualified Command.MinCopies
import qualified Command.Trust
import qualified Command.Untrust
import qualified Command.Semitrust
import qualified Command.Dead
import qualified Command.Group
import qualified Command.Wanted
import qualified Command.GroupWanted
import qualified Command.Required
import qualified Command.Schedule
import qualified Command.Ungroup
import qualified Command.Config
import qualified Command.Vicfg
import qualified Command.Sync
import qualified Command.Assist
import qualified Command.Pull
import qualified Command.Push
import qualified Command.Satisfy
import qualified Command.Mirror
import qualified Command.AddUrl
import qualified Command.ImportFeed
import qualified Command.RmUrl
import qualified Command.Import
import qualified Command.Export
import qualified Command.Map
import qualified Command.Direct
import qualified Command.Indirect
import qualified Command.Upgrade
import qualified Command.Forget
import qualified Command.P2P
import qualified Command.Proxy
import qualified Command.DiffDriver
import qualified Command.Smudge
import qualified Command.FilterProcess
import qualified Command.Restage
import qualified Command.Undo
import qualified Command.Version
import qualified Command.RemoteDaemon
#ifdef WITH_ASSISTANT
import qualified Command.Watch
import qualified Command.Assistant
#ifdef WITH_WEBAPP
import qualified Command.WebApp
#endif
#endif
import qualified Command.Test
import qualified Command.FuzzTest
import qualified Command.TestRemote
import qualified Command.Benchmark

cmds :: Parser TestOptions -> TestRunner -> MkBenchmarkGenerator -> [Command]
cmds testoptparser testrunner mkbenchmarkgenerator = map addGitAnnexCommonOptions $
	[ Command.Help.cmd
	, Command.Add.cmd
	, Command.Get.cmd
	, Command.Drop.cmd
	, Command.Move.cmd
	, Command.Copy.cmd
	, Command.Fsck.cmd
	, Command.Unlock.cmd
	, Command.Unlock.editcmd
	, Command.Lock.cmd
	, Command.Sync.cmd
	, Command.Assist.cmd
	, Command.Pull.cmd
	, Command.Push.cmd
	, Command.Satisfy.cmd
	, Command.Mirror.cmd
	, Command.AddUrl.cmd
	, Command.ImportFeed.cmd
	, Command.RmUrl.cmd
	, Command.Import.cmd
	, Command.Export.cmd
	, Command.Init.cmd
	, Command.Describe.cmd
	, Command.InitRemote.cmd
	, Command.EnableRemote.cmd
	, Command.ConfigRemote.cmd
	, Command.RenameRemote.cmd
	, Command.EnableTor.cmd
	, Command.Multicast.cmd
	, Command.Reinject.cmd
	, Command.Unannex.cmd
	, Command.Uninit.cmd
	, Command.Reinit.cmd
	, Command.PreCommit.cmd
	, Command.PostReceive.cmd
	, Command.NumCopies.cmd
	, Command.MinCopies.cmd
	, Command.Trust.cmd
	, Command.Untrust.cmd
	, Command.Semitrust.cmd
	, Command.Dead.cmd
	, Command.Group.cmd
	, Command.Wanted.cmd
	, Command.GroupWanted.cmd
	, Command.Required.cmd
	, Command.Schedule.cmd
	, Command.Ungroup.cmd
	, Command.Config.cmd
	, Command.Vicfg.cmd
	, Command.LookupKey.cmd
	, Command.CalcKey.cmd
	, Command.ContentLocation.cmd
	, Command.ExamineKey.cmd
	, Command.MatchExpression.cmd
	, Command.FromKey.cmd
	, Command.RegisterUrl.cmd
	, Command.UnregisterUrl.cmd
	, Command.SetKey.cmd
	, Command.DropKey.cmd
	, Command.Transferrer.cmd
	, Command.TransferKey.cmd
	, Command.TransferKeys.cmd
	, Command.SetPresentKey.cmd
	, Command.ReadPresentKey.cmd
	, Command.CheckPresentKey.cmd
	, Command.ReKey.cmd
	, Command.Adjust.cmd
	, Command.MetaData.cmd
	, Command.View.cmd
	, Command.VAdd.cmd
	, Command.VFilter.cmd
	, Command.VPop.cmd
	, Command.VCycle.cmd
	, Command.Fix.cmd
	, Command.Expire.cmd
	, Command.Repair.cmd
	, Command.Unused.cmd
	, Command.DropUnused.cmd
	, Command.AddUnused.cmd
	, Command.FilterBranch.cmd
	, Command.Find.cmd
	, Command.FindKeys.cmd
	, Command.FindRef.cmd
	, Command.Whereis.cmd
	, Command.WhereUsed.cmd
	, Command.List.cmd
	, Command.Log.cmd
	, Command.Merge.cmd
	, Command.ResolveMerge.cmd
	, Command.Info.cmd
	, Command.Status.cmd
	, Command.Inprogress.cmd
	, Command.Migrate.cmd
	, Command.Map.cmd
	, Command.Direct.cmd
	, Command.Indirect.cmd
	, Command.Upgrade.cmd
	, Command.Forget.cmd
	, Command.P2P.cmd
	, Command.Proxy.cmd
	, Command.DiffDriver.cmd
	, Command.Smudge.cmd
	, Command.FilterProcess.cmd
	, Command.Restage.cmd
	, Command.Undo.cmd
	, Command.Version.cmd
	, Command.RemoteDaemon.cmd
#ifdef WITH_ASSISTANT
	, Command.Watch.cmd
	, Command.Assistant.cmd
#ifdef WITH_WEBAPP
	, Command.WebApp.cmd
#endif
#endif
	, Command.Test.cmd testoptparser testrunner
	, Command.FuzzTest.cmd
	, Command.TestRemote.cmd
	, Command.Benchmark.cmd $
		mkbenchmarkgenerator $ cmds testoptparser testrunner (\_ _ -> return noop)
	]

addGitAnnexCommonOptions :: Command -> Command
addGitAnnexCommonOptions c = c { cmdannexoptions = gitAnnexCommonOptions ++ cmdannexoptions c }

run :: Parser TestOptions -> TestRunner -> MkBenchmarkGenerator -> [String] -> IO ()
run testoptparser testrunner mkbenchmarkgenerator args = go envmodes
  where
	go [] = dispatch True True args 
		(cmds testoptparser testrunner mkbenchmarkgenerator)
		[] Git.CurrentRepo.get
		"git-annex"
		"manage files with git, without checking their contents in"
	go ((v, a):rest) = maybe (go rest) a =<< getEnv v
	envmodes =
		[ (sshOptionsEnv, runSshOptions args)
		, (sshAskPassEnv, runSshAskPass)
		, (multicastReceiveEnv, runMulticastReceive args)
		]
