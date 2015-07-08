{- git-annex command infrastructure
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command (
	command,
	commandParser,
	noRepo,
	noCommit,
	noMessages,
	withOptions,
	next,
	stop,
	stopUnless,
	whenAnnexed,
	ifAnnexed,
	isBareRepo,
	module ReExported
) where

import Common.Annex
import qualified Backend
import qualified Git
import Types.Command as ReExported
import Types.Option as ReExported
import CmdLine.Seek as ReExported
import Checks as ReExported
import CmdLine.Usage as ReExported
import CmdLine.Action as ReExported
import CmdLine.Option as ReExported
import CmdLine.GitAnnex.Options as ReExported

import qualified Options.Applicative as O

{- Generates a normal Command -}
command :: String -> String -> CommandSection -> String -> (Command -> CommandParser) -> Command
command name paramdesc section desc parser = c
  where
	c = Command [] Nothing commonChecks False False name paramdesc section desc (parser c)

{- Simple CommandParser generator, for when the CommandSeek wants all
 - non-option parameters. -}
commandParser :: (CmdParams -> CommandSeek) -> Command -> CommandParser
commandParser mkseek c = mkseek <$> O.many cmdparams
  where
	cmdparams = O.argument O.str (O.metavar (cmdparamdesc c))

{- Indicates that a command doesn't need to commit any changes to
 - the git-annex branch. -}
noCommit :: Command -> Command
noCommit c = c { cmdnocommit = True }

{- Indicates that a command should not output anything other than what
 - it directly sends to stdout. (--json can override this). -}
noMessages :: Command -> Command
noMessages c = c { cmdnomessages = True }

{- Adds a fallback action to a command, that will be run if it's used
 - outside a git repository. -}
noRepo :: (CmdParams -> IO ()) -> Command -> Command
noRepo a c = c { cmdnorepo = Just a }

{- Adds options to a command. -}
withOptions :: [Option] -> Command -> Command
withOptions o c = c { cmdoptions = cmdoptions c ++ o }

{- For start and perform stages to indicate what step to run next. -}
next :: a -> Annex (Maybe a)
next a = return $ Just a

{- Or to indicate nothing needs to be done. -}
stop :: Annex (Maybe a)
stop = return Nothing

{- Stops unless a condition is met. -}
stopUnless :: Annex Bool -> Annex (Maybe a) -> Annex (Maybe a)
stopUnless c a = ifM c ( a , stop )

{- Modifies an action to only act on files that are already annexed,
 - and passes the key on to it. -}
whenAnnexed :: (FilePath -> Key -> Annex (Maybe a)) -> FilePath -> Annex (Maybe a)
whenAnnexed a file = ifAnnexed file (a file) (return Nothing)

ifAnnexed :: FilePath -> (Key -> Annex a) -> Annex a -> Annex a
ifAnnexed file yes no = maybe no yes =<< Backend.lookupFile file

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare
