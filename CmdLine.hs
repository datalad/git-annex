{- git-annex command line parsing and dispatch
 -
 - Copyright 2010-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine (
	dispatch,
	usage,
	parseCmd,
	prepRunCommand,
) where

import qualified Options.Applicative as O
import qualified Options.Applicative.Help as H
import Control.Exception (throw)

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.AutoCorrect
import qualified Git.Config
import Annex.Action
import Annex.Environment
import Command
import Types.Messages

{- Runs the passed command line. -}
dispatch :: Bool -> CmdParams -> [Command] -> [(String, String)] -> IO Git.Repo -> String -> String -> IO ()
dispatch fuzzyok allargs allcmds fields getgitrepo progname progdesc = do
	setupConsole
	go =<< tryNonAsync getgitrepo
  where
	go (Right g) = do
		state <- Annex.new g
		Annex.eval state $ do
			checkEnvironment
			forM_ fields $ uncurry Annex.setField
			(cmd, seek, globalconfig) <- parsewith False cmdparser
				(\a -> inRepo $ a . Just)
				(liftIO . O.handleParseResult)
			prepRunCommand cmd globalconfig
			startup
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
		case parseCmd progname progdesc allargs allcmds getparser of
			O.Failure _ -> do
				-- parse failed, so fall back to
				-- fuzzy matching, or to showing usage
				when (fuzzy && not secondrun) $
					ingitrepo autocorrect
				handleresult (parseCmd progname progdesc correctedargs allcmds getparser)
			res -> handleresult res
	  where
		autocorrect = Git.AutoCorrect.prepare (fromJust inputcmdname) cmdname cmds
		name
			| fuzzy = case cmds of
				(c:_) -> Just (cmdname c)
				_ -> inputcmdname
			| otherwise = inputcmdname
		correctedargs = case name of
			Nothing -> allargs
			Just n -> n:args
	
	(inputcmdname, args) = findCmdName allargs
	(fuzzy, cmds) = findCmd fuzzyok allcmds inputcmdname

{- Parses command line, selecting one of the commands from the list. -}
parseCmd :: String -> String -> CmdParams -> [Command] -> (Command -> O.Parser v) -> O.ParserResult (Command, v, GlobalSetter)
parseCmd progname progdesc allargs allcmds getparser = 
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
		<*> combineGlobalOptions (cmdglobaloptions c)
	synopsis n d = n ++ " - " ++ d
	intro = mconcat $ concatMap (\l -> [H.text l, H.line])
		(synopsis progname progdesc : commandList allcmds)

{- Finds the Command that matches the subcommand name.
 - Does fuzzy matching if necessary, which may result in multiple Commands. -}
findCmd :: Bool -> [Command] -> Maybe String -> (Bool, [Command])
findCmd fuzzyok cmds (Just n)
	| not (null exactcmds) = (False, exactcmds)
	| fuzzyok && not (null inexactcmds) = (True, inexactcmds)
	| otherwise = (False, [])
  where
	exactcmds = filter (\c -> cmdname c == n) cmds
	inexactcmds = Git.AutoCorrect.fuzzymatches n cmdname cmds
findCmd _ _ Nothing = (False, [])

{- Parses command line params far enough to find the subcommand name. -}
findCmdName :: CmdParams -> (Maybe String, CmdParams)
findCmdName argv = (name, args)
  where
	(name, args) = findname argv []
	findname [] c = (Nothing, reverse c)
	findname (a:as) c
		| "-" `isPrefixOf` a = findname as (a:c)
		| otherwise = (Just a, reverse c ++ as)

prepRunCommand :: Command -> GlobalSetter -> Annex ()
prepRunCommand cmd globalconfig = do
	when (cmdnomessages cmd) $
		Annex.setOutput QuietOutput
	getParsed globalconfig
	whenM (annexDebug <$> Annex.getGitConfig) $
		liftIO enableDebugOutput
