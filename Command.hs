{- git-annex command infrastructure
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command (
	module Command,
	module ReExported
) where

import Annex.Common as ReExported
import Types.Command as ReExported
import Types.DeferredParse as ReExported
import CmdLine.Seek as ReExported
import CmdLine.Usage as ReExported
import CmdLine.Action as ReExported
import CmdLine.Option as ReExported
import CmdLine.AnnexSetter as ReExported
import CmdLine.GitAnnex.Options as ReExported
import CmdLine.Batch as ReExported
import Options.Applicative as ReExported hiding (command)
import Annex.RepoSize.LiveUpdate as ReExported
import qualified Git
import Annex.Init
import Annex.Startup
import Utility.Daemon
import Types.Transfer
import Types.ActionItem as ReExported
import Types.WorkerPool as ReExported
import Remote.List

{- Generates a normal Command -}
command :: String -> CommandSection -> String -> CmdParamsDesc -> (CmdParamsDesc -> CommandParser) -> Command
command name section desc paramdesc mkparser =
	Command commonChecks False False name paramdesc 
		section desc (mkparser paramdesc) mempty [] Nothing

{- Simple option parser that takes all non-option params as-is. -}
withParams :: (CmdParams -> v) -> CmdParamsDesc -> Parser v
withParams mkseek paramdesc = mkseek <$> cmdParams paramdesc

withParams' :: (CmdParams -> v) -> Mod ArgumentFields String -> String -> Parser v
withParams' mkseek completers paramdesc = mkseek
	<$> cmdParamsWithCompleter paramdesc completers

{- Uses the supplied option parser, which yields a deferred parse,
 - and calls finishParse on the result before passing it to the
 - CommandSeek constructor. -}
(<--<) :: DeferredParseClass a
	=> (a -> CommandSeek) 
	-> (CmdParamsDesc -> Parser a)
	-> CmdParamsDesc
	-> Parser CommandSeek
(<--<) mkseek optparser paramsdesc = 
	(mkseek <=< finishParse) <$> optparser paramsdesc

{- Indicates that a command doesn't need to commit any changes to
 - the git-annex branch. -}
noCommit :: Command -> Command
noCommit c = c { cmdnocommit = True }

{- Indicates that a command should not output the usual messages when
 - starting or stopping processing a file or other item. Unless --json mode
 - is enabled, this also enables quiet output mode, so only things
 - explicitly output by the command are shown and not progress messages
 - etc.
 -}
noMessages :: Command -> Command
noMessages c = c { cmdnomessages = True }

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: (CmdParamsDesc -> Parser (IO ())) -> Command -> Command
noRepo a c = c { cmdnorepo = Just (a (cmdparamdesc c)) }

{- Adds Annex options to a command. -}
withAnnexOptions :: [[AnnexOption]] -> Command -> Command
withAnnexOptions os c = c { cmdannexoptions = cmdannexoptions c ++ concat os }

{- For start stage to indicate what will be done. -}
starting:: MkActionItem actionitem => String -> actionitem -> SeekInput -> CommandPerform -> CommandStart
starting msg ai si a = next
	(StartMessage msg (mkActionItem ai) si, a)

{- Use when noMessages was used but the command is going to output
 - usual messages after all. -}
startingUsualMessages :: MkActionItem t => String -> t -> SeekInput -> CommandPerform -> CommandStart
startingUsualMessages msg t si a = next 
	(StartUsualMessages msg (mkActionItem t) si, a)

{- When no message should be displayed at start/end, but messages can still 
 - be displayed when using eg includeCommandAction. -}
startingNoMessage :: MkActionItem t => t -> CommandPerform -> CommandStart
startingNoMessage t a = next (StartNoMessage (mkActionItem t), a)

{- For commands that do not display usual start or end messages, 
 - but have some other custom output. -}
startingCustomOutput :: MkActionItem t => t -> CommandPerform -> CommandStart
startingCustomOutput t a = next (CustomOutput (mkActionItem t), a)

{- For perform stage to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- For start and perform stage to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

{- When doing a dry run, avoid actually performing the action, but pretend
 - that it succeeded. -}
skipWhenDryRun :: DryRun -> CommandPerform -> CommandPerform
skipWhenDryRun (DryRun False) a = a
skipWhenDryRun (DryRun True) _ = next $ return True

{- When acting on a failed transfer, stops unless it was in the specified
 - direction. -}
checkFailedTransferDirection :: ActionItem -> Direction -> Annex (Maybe a) -> Annex (Maybe a)
checkFailedTransferDirection ai d = stopUnless (pure check)
  where
	check = case actionItemTransferDirection ai of
		Nothing -> True
		Just d' -> d' == d

commonChecks :: [CommandCheck]
commonChecks = [repoExists]

repoExists :: CommandCheck
repoExists = CommandCheck RepoExists (ensureInitialized startupAnnex remoteList)

notBareRepo :: Command -> Command
notBareRepo = addCheck CheckNotBareRepo checkNotBareRepo

checkNotBareRepo :: Annex ()
checkNotBareRepo = whenM (fromRepo Git.repoIsLocalBare) $
	giveup "You cannot run this command in a bare repository."

noDaemonRunning :: Command -> Command
noDaemonRunning = addCheck NoDaemonRunning $ whenM (isJust <$> daemonpid) $
	giveup "You cannot run this command while git-annex watch or git-annex assistant is running."
  where
	daemonpid = liftIO . checkDaemon . fromRawFilePath
		=<< fromRepo gitAnnexPidFile

dontCheck :: CommandCheck -> Command -> Command
dontCheck check cmd = mutateCheck cmd $ \c -> filter (/= check) c

addCheck :: CommandCheckId -> Annex () -> Command -> Command
addCheck cid check cmd = mutateCheck cmd $ \c ->
	CommandCheck cid check : c

mutateCheck :: Command -> ([CommandCheck] -> [CommandCheck]) -> Command
mutateCheck cmd@(Command { cmdcheck = c }) a = cmd { cmdcheck = a c }
