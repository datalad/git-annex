{- git-annex command infrastructure
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	command,
	withParams,
	(<--<),
	noRepo,
	noCommit,
	noMessages,
	withGlobalOptions,
	next,
	stop,
	stopUnless,
	whenAnnexed,
	ifAnnexed,
	lookupFile,
	isBareRepo,
	module ReExported
) where

import Annex.Common as ReExported
import Annex.WorkTree
import qualified Git
import Types.Command as ReExported
import Types.DeferredParse as ReExported
import Checks as ReExported
import CmdLine.Seek as ReExported
import CmdLine.Usage as ReExported
import CmdLine.Action as ReExported
import CmdLine.Option as ReExported
import CmdLine.GlobalSetter as ReExported
import CmdLine.GitAnnex.Options as ReExported
import CmdLine.Batch as ReExported
import Options.Applicative as ReExported hiding (command)

import qualified Options.Applicative as O

{- Generates a normal Command -}
command :: String -> CommandSection -> String -> CmdParamsDesc -> (CmdParamsDesc -> CommandParser) -> Command
command name section desc paramdesc mkparser =
	Command commonChecks False False name paramdesc 
		section desc (mkparser paramdesc) Nothing

{- Simple option parser that takes all non-option params as-is. -}
withParams :: (CmdParams -> v) -> CmdParamsDesc -> O.Parser v
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

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: (String -> O.Parser (IO ())) -> Command -> Command
noRepo a c = c { cmdnorepo = Just (a (cmdparamdesc c)) }

{- Adds global options to a command's option parser, and modifies its seek
 - option to first run actions for them.
 -}
withGlobalOptions :: [GlobalOption] -> Command -> Command
withGlobalOptions os c = c { cmdparser = apply <$> mixin (cmdparser c) }
  where
	mixin p = (,) 
		<$> p
		<*> combineGlobalOptions os
	apply (seek, globalsetters) = do
		void $ getParsed globalsetters
		seek

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare
