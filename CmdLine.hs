{- git-annex command line parsing and dispatch
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
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
import Control.Monad.IO.Class (MonadIO)
import System.Exit

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.AutoCorrect
import qualified Git.Config
import Annex.Action
import Annex.Environment
import Command
import Types.Messages

{- Parses input arguments, finds a matching Command, and runs it. -}
dispatch :: Bool -> Bool -> CmdParams -> [Command] -> [(String, String)] -> IO Git.Repo -> String -> String -> IO ()
dispatch addonok fuzzyok allargs allcmds fields getgitrepo progname progdesc =
	go addonok allcmds $
		findAddonCommand subcommandname >>= \case
			Just c -> go addonok (c:allcmds) noop
			Nothing -> go addonok allcmds $
				findAllAddonCommands >>= \cs ->
					go False (cs++allcmds) noop
  where
	go p allcmds' cont =
		let (fuzzy, cmds) = selectCmd fuzzyok allcmds' subcommandname
		in if not p || (not fuzzy && not (null cmds))
			then dispatch' subcommandname args fuzzy cmds allargs allcmds' fields getgitrepo progname progdesc
			else cont
	
	(subcommandname, args) = subCmdName allargs

dispatch' :: (Maybe String) -> CmdParams -> Bool -> [Command] -> CmdParams -> [Command] -> [(String, String)] -> IO Git.Repo -> String -> String -> IO ()
dispatch' subcommandname args fuzzy cmds allargs allcmds fields getgitrepo progname progdesc = do
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
		autocorrect = Git.AutoCorrect.prepare (fromJust subcommandname) cmdname cmds
		name
			| fuzzy = case cmds of
				(c:_) -> Just (cmdname c)
				_ -> subcommandname
			| otherwise = subcommandname
		correctedargs = case name of
			Nothing -> allargs
			Just n -> n:args

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
		<> cmdinfomod c
	mkparser c = (,,) 
		<$> pure c
		<*> getparser c
		<*> parserGlobalOptions (cmdglobaloptions c)
	synopsis n d = n ++ " - " ++ d
	intro = mconcat $ concatMap (\l -> [H.text l, H.line])
		(synopsis progname progdesc : commandList allcmds)

{- Selects the Command that matches the subcommand name.
 - Does fuzzy matching if necessary, which may result in multiple Commands. -}
selectCmd :: Bool -> [Command] -> Maybe String -> (Bool, [Command])
selectCmd fuzzyok cmds (Just n)
	| not (null exactcmds) = (False, exactcmds)
	| fuzzyok && not (null inexactcmds) = (True, inexactcmds)
	| otherwise = (False, [])
  where
	exactcmds = filter (\c -> cmdname c == n) cmds
	inexactcmds = Git.AutoCorrect.fuzzymatches n cmdname cmds
selectCmd _ _ Nothing = (False, [])

{- Parses command line params far enough to find the subcommand name. -}
subCmdName :: CmdParams -> (Maybe String, CmdParams)
subCmdName argv = (name, args)
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
		enableDebugOutput

findAddonCommand :: Maybe String -> IO (Maybe Command)
findAddonCommand Nothing = return Nothing
findAddonCommand (Just subcommandname) =
	searchPath c >>= \case
		Nothing -> return Nothing
		Just p -> return (Just (mkAddonCommand p subcommandname))
  where
	c = "git-annex-" ++ subcommandname

findAllAddonCommands :: IO [Command]
findAllAddonCommands = 
	filter isaddoncommand
		. map (\p -> mkAddonCommand p (deprefix p))
		<$> searchPathContents ("git-annex-" `isPrefixOf`)
  where
	deprefix = replace "git-annex-" "" . takeFileName
	isaddoncommand c
		-- git-annex-shell
		| cmdname c == "shell" = False
		-- external special remotes
		| "remote-" `isPrefixOf` cmdname c = False
		-- external backends
		| "backend-" `isPrefixOf` cmdname c = False
		| otherwise = True

mkAddonCommand :: FilePath -> String -> Command
mkAddonCommand p subcommandname = Command
	{ cmdcheck = []
	, cmdnocommit = True
	, cmdnomessages = True
	, cmdname = subcommandname
	, cmdparamdesc = "[PARAMS]"
	, cmdsection = SectionAddOn
	, cmddesc = "addon command"
	, cmdglobaloptions = []
	, cmdinfomod = O.forwardOptions
	, cmdparser = parse
	, cmdnorepo = Just parse
	}
  where
	parse :: (Monad m, MonadIO m) => Parser (m ())
	parse = (liftIO . run) <$> cmdParams "PARAMS"

	run ps = withCreateProcess (proc p ps) $ \_ _ _ pid ->
		exitWith =<< waitForProcess pid
