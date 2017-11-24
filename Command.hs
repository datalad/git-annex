{- git-annex command infrastructure
 -
 - Copyright 2010-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	module Command,
	module ReExported
) where

import Annex.Common as ReExported
import Annex.WorkTree as ReExported (whenAnnexed, ifAnnexed)
import Types.Command as ReExported
import Types.DeferredParse as ReExported
import CmdLine.Seek as ReExported
import CmdLine.Usage as ReExported
import CmdLine.Action as ReExported
import CmdLine.Option as ReExported
import CmdLine.GlobalSetter as ReExported
import CmdLine.GitAnnex.Options as ReExported
import CmdLine.Batch as ReExported
import Options.Applicative as ReExported hiding (command)
import qualified Annex
import qualified Git
import Annex.Init
import Config
import Utility.Daemon
import Types.Transfer
import Types.ActionItem
import Types.Messages

{- Generates a normal Command -}
command :: String -> CommandSection -> String -> CmdParamsDesc -> (CmdParamsDesc -> CommandParser) -> Command
command name section desc paramdesc mkparser =
	Command commonChecks False False name paramdesc 
		section desc (mkparser paramdesc) [] Nothing

{- Simple option parser that takes all non-option params as-is. -}
withParams :: (CmdParams -> v) -> CmdParamsDesc -> Parser v
withParams mkseek paramdesc = mkseek <$> cmdParams paramdesc

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
 - etc. -}
noMessages :: Command -> Command
noMessages c = c { cmdnomessages = True }

{- Undoes noMessages -}
allowMessages :: Annex ()
allowMessages = do
	curr <- Annex.getState Annex.output
	case outputType curr of
		QuietOutput -> Annex.setOutput NormalOutput
		_ -> noop
	Annex.changeState $ \s -> s
		{ Annex.output = (Annex.output s) { implicitMessages = True } }

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: (String -> Parser (IO ())) -> Command -> Command
noRepo a c = c { cmdnorepo = Just (a (cmdparamdesc c)) }

{- Adds global options to a command's. -}
withGlobalOptions :: [GlobalOption] -> Command -> Command
withGlobalOptions os c = c { cmdglobaloptions = cmdglobaloptions c ++ os }

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

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
repoExists = CommandCheck 0 ensureInitialized

notDirect :: Command -> Command
notDirect = addCheck $ whenM isDirect $
	giveup "You cannot run this command in a direct mode repository."

notBareRepo :: Command -> Command
notBareRepo = addCheck $ whenM (fromRepo Git.repoIsLocalBare) $
	giveup "You cannot run this command in a bare repository."

noDaemonRunning :: Command -> Command
noDaemonRunning = addCheck $ whenM (isJust <$> daemonpid) $
	giveup "You cannot run this command while git-annex watch or git-annex assistant is running."
  where
	daemonpid = liftIO . checkDaemon =<< fromRepo gitAnnexPidFile

dontCheck :: CommandCheck -> Command -> Command
dontCheck check cmd = mutateCheck cmd $ \c -> filter (/= check) c

addCheck :: Annex () -> Command -> Command
addCheck check cmd = mutateCheck cmd $ \c ->
	CommandCheck (length c + 100) check : c

mutateCheck :: Command -> ([CommandCheck] -> [CommandCheck]) -> Command
mutateCheck cmd@(Command { cmdcheck = c }) a = cmd { cmdcheck = a c }
