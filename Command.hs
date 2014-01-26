{- git-annex command infrastructure
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Command (
	command,
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
	checkAuto,
	module ReExported
) where

import Common.Annex
import qualified Backend
import qualified Annex
import qualified Git
import Types.Command as ReExported
import Types.Option as ReExported
import CmdLine.Seek as ReExported
import Checks as ReExported
import CmdLine.Usage as ReExported
import RunCommand as ReExported
import CmdLine.Option as ReExported
import CmdLine.GitAnnex.Options as ReExported

{- Generates a normal command -}
command :: String -> String -> CommandSeek -> CommandSection -> String -> Command
command = Command [] Nothing commonChecks False False

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
withOptions o c = c { cmdoptions = o }

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
 - and passes the key and backend on to it. -}
whenAnnexed :: (FilePath -> (Key, Backend) -> Annex (Maybe a)) -> FilePath -> Annex (Maybe a)
whenAnnexed a file = ifAnnexed file (a file) (return Nothing)

ifAnnexed :: FilePath -> ((Key, Backend) -> Annex a) -> Annex a -> Annex a
ifAnnexed file yes no = maybe no yes =<< Backend.lookupFile file

isBareRepo :: Annex Bool
isBareRepo = fromRepo Git.repoIsLocalBare

checkAuto :: Annex Bool -> Annex Bool
checkAuto checker = ifM (Annex.getState Annex.auto)
	( checker , return True )
