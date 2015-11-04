{- git-annex command line parsing and dispatch
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (
	dispatch,
	usage,
) where

import qualified Options.Applicative as O
import qualified Options.Applicative.Help as H
import qualified Control.Exception as E
import Control.Exception (throw)

import Common.Annex
import qualified Annex
import qualified Git
import qualified Git.AutoCorrect
import qualified Git.Config
import Annex.Action
import Annex.Environment
import Command
import Types.Messages
import Messages.Concurrent

{- Runs the passed command line. -}
dispatch :: Bool -> CmdParams -> [Command] -> [GlobalOption] -> [(String, String)] -> IO Git.Repo -> String -> String -> IO ()
dispatch fuzzyok allargs allcmds globaloptions fields getgitrepo progname progdesc = do
	setupConsole
	go =<< (E.try getgitrepo :: IO (Either E.SomeException Git.Repo))
  where
	go (Right g) = do
		state <- Annex.new g
		Annex.eval state $ do
			checkEnvironment
			forM_ fields $ uncurry Annex.setField
			(cmd, seek, globalconfig) <- parsewith False cmdparser
				(\a -> inRepo $ a . Just)
				(liftIO . O.handleParseResult)
			when (cmdnomessages cmd) $ 
				Annex.setOutput QuietOutput
			getParsed globalconfig
			whenM (annexDebug <$> Annex.getGitConfig) $
				liftIO enableDebugOutput
			startup
			withConcurrentOutput $
				performCommandAction cmd seek $
					shutdown $ cmdnocommit cmd
	go (Left norepo) = do
		let ingitrepo = \a -> a =<< Git.Config.global
		-- Parse command line with full cmdparser first,
		-- so that help can be displayed for bad parses
		-- even when not run in a repo.
		res <- parsewith False cmdparser ingitrepo return
		case res of
			Failure _ -> void (O.handleParseResult res)
			_ -> do
				-- Parse command line in norepo mode.
				(_, a, _globalconfig) <- parsewith True
					(fromMaybe (throw norepo) . cmdnorepo)
					ingitrepo
					O.handleParseResult
				a

	parsewith secondrun getparser ingitrepo handleresult =
		case parseCmd progname progdesc globaloptions allargs allcmds getparser of
			O.Failure _ -> do
				-- parse failed, so fall back to
				-- fuzzy matching, or to showing usage
				when (fuzzy && not secondrun) $
					ingitrepo autocorrect
				handleresult (parseCmd progname progdesc globaloptions correctedargs allcmds getparser)
			res -> handleresult res
	  where
		autocorrect = Git.AutoCorrect.prepare (fromJust inputcmdname) cmdname cmds
		(fuzzy, cmds, inputcmdname, args) = findCmd fuzzyok allargs allcmds
		name
			| fuzzy = case cmds of
				(c:_) -> Just (cmdname c)
				_ -> inputcmdname
			| otherwise = inputcmdname
		correctedargs = case name of
			Nothing -> allargs
			Just n -> n:args

{- Parses command line, selecting one of the commands from the list. -}
parseCmd :: String -> String -> [GlobalOption] -> CmdParams -> [Command] -> (Command -> O.Parser v) -> O.ParserResult (Command, v, GlobalSetter)
parseCmd progname progdesc globaloptions allargs allcmds getparser = 
	O.execParserPure (O.prefs O.idm) pinfo allargs
  where
	pinfo = O.info (O.helper <*> subcmds) (O.progDescDoc (Just intro))
	subcmds = O.hsubparser $ mconcat $ map mkcommand allcmds
	mkcommand c = O.command (cmdname c) $ O.info (mkparser c) $ O.fullDesc 
		<> O.header (synopsis (progname ++ " " ++ cmdname c) (cmddesc c))
		<> O.footer ("For details, run: " ++ progname ++ " help " ++ cmdname c)
	mkparser c = (,,) 
		<$> pure c
		<*> getparser c
		<*> combineGlobalOptions globaloptions
	synopsis n d = n ++ " - " ++ d
	intro = mconcat $ concatMap (\l -> [H.text l, H.line])
		(synopsis progname progdesc : commandList allcmds)

{- Parses command line params far enough to find the Command to run, and
 - returns the remaining params.
 - Does fuzzy matching if necessary, which may result in multiple Commands. -}
findCmd :: Bool -> CmdParams -> [Command] -> (Bool, [Command], Maybe String, CmdParams)
findCmd fuzzyok argv cmds
	| not (null exactcmds) = ret (False, exactcmds)
	| fuzzyok && not (null inexactcmds) = ret (True, inexactcmds)
	| otherwise = ret (False, [])
  where
	ret (fuzzy, matches) = (fuzzy, matches, name, args)
	(name, args) = findname argv []
	findname [] c = (Nothing, reverse c)
	findname (a:as) c
		| "-" `isPrefixOf` a = findname as (a:c)
		| otherwise = (Just a, reverse c ++ as)
	exactcmds = filter (\c -> name == Just (cmdname c)) cmds
	inexactcmds = case name of
		Nothing -> []
		Just n -> Git.AutoCorrect.fuzzymatches n cmdname cmds
