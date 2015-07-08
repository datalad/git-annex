{- git-annex command line parsing and dispatch
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module CmdLine (
	dispatch,
	usage,
	shutdown
) where

import qualified Control.Exception as E
import qualified Data.Map as M
import Control.Exception (throw)
import qualified Options.Applicative as O
#ifndef mingw32_HOST_OS
import System.Posix.Signals
#endif

import Common.Annex
import qualified Annex
import qualified Git
import qualified Git.AutoCorrect
import qualified Git.Config
import Annex.Content
import Annex.Environment
import Command
import Types.Messages

{- Runs the passed command line. -}
dispatch :: Bool -> CmdParams -> [Command] -> [Option] -> [(String, String)] -> String -> IO Git.Repo -> IO ()
dispatch fuzzyok allargs allcmds commonoptions fields header getgitrepo = do
	setupConsole
	go =<< (E.try getgitrepo :: IO (Either E.SomeException Git.Repo))
  where
	go (Right g) = do
		state <- Annex.new g
		Annex.eval state $ do
			checkEnvironment
			when fuzzy $
				inRepo $ autocorrect . Just
			forM_ fields $ uncurry Annex.setField
			(cmd, seek) <- liftIO $
				O.handleParseResult (parseCmd (name:args) allcmds cmdparser)
			when (cmdnomessages cmd) $ 
				Annex.setOutput QuietOutput
			-- TODO: propigate global options to annex state (how?)
			whenM (annexDebug <$> Annex.getGitConfig) $
				liftIO enableDebugOutput
			startup
			performCommandAction cmd seek $
				shutdown $ cmdnocommit cmd
	go (Left norepo) = do
		when fuzzy $
			autocorrect =<< Git.Config.global
		let norepoparser = fromMaybe (throw norepo) . cmdnorepo
		(_cmd, a) <- O.handleParseResult (parseCmd (name:args) allcmds norepoparser)
		a

	autocorrect = Git.AutoCorrect.prepare inputcmdname cmdname cmds
	err msg = msg ++ "\n\n" ++ usage header allcmds
	(fuzzy, cmds, inputcmdname, args) = findCmd fuzzyok allargs allcmds err
	name
		| fuzzy = case cmds of
			[c] -> cmdname c
			_ -> inputcmdname
		| otherwise = inputcmdname


{- Parses command line, selecting one of the commands from the list. -}
parseCmd :: CmdParams -> [Command] -> (Command -> O.Parser v) -> O.ParserResult (Command, v)
parseCmd allargs allcmds getparser = O.execParserPure (O.prefs O.idm) pinfo allargs
  where
	pinfo = O.info (O.subparser $ mconcat $ map mkcommand allcmds)
		( O.fullDesc
		<> O.progDesc "hiya"
		<> O.header "ook - aaa"
		)
	mkcommand c = O.command (cmdname c) $ O.info (mkparser c) 
		(O.fullDesc <> O.progDesc (cmddesc c))
	mkparser c = (,)
		<$> pure c
		<*> getparser c

{- Parses command line params far enough to find the Command to run, and
 - returns the remaining params.
 - Does fuzzy matching if necessary, which may result in multiple Commands. -}
findCmd :: Bool -> CmdParams -> [Command] -> (String -> String) -> (Bool, [Command], String, CmdParams)
findCmd fuzzyok argv cmds err
	| isNothing name = error $ err "missing command"
	| not (null exactcmds) = (False, exactcmds, fromJust name, args)
	| fuzzyok && not (null inexactcmds) = (True, inexactcmds, fromJust name, args)
	| otherwise = error $ err $ "unknown command " ++ fromJust name
  where
	(name, args) = findname argv []
	findname [] c = (Nothing, reverse c)
	findname (a:as) c
		| "-" `isPrefixOf` a = findname as (a:c)
		| otherwise = (Just a, reverse c ++ as)
	exactcmds = filter (\c -> name == Just (cmdname c)) cmds
	inexactcmds = case name of
		Nothing -> []
		Just n -> Git.AutoCorrect.fuzzymatches n cmdname cmds

{- Actions to perform each time ran. -}
startup :: Annex ()
startup =
#ifndef mingw32_HOST_OS
	liftIO $ void $ installHandler sigINT Default Nothing
#else
	return ()
#endif

{- Cleanup actions. -}
shutdown :: Bool -> Annex ()
shutdown nocommit = do
	saveState nocommit
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
	liftIO reapZombies -- zombies from long-running git processes
