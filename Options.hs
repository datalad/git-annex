{- git-annex command-line options
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Options where

import System.Console.GetOpt
import System.Log.Logger

import Common.Annex
import qualified Annex
import Limit

{- Each dashed command-line option results in generation of an action
 - in the Annex monad that performs the necessary setting.
 -}
type Option = OptDescr (Annex ())

commonOptions :: [Option]
commonOptions =
	[ Option [] ["force"] (NoArg (setforce True))
		"allow actions that may lose annexed data"
	, Option ['F'] ["fast"] (NoArg (setfast True))
		"avoid slow operations"
	, Option ['a'] ["auto"] (NoArg (setauto True))
		"automatic mode"
	, Option ['q'] ["quiet"] (NoArg (setoutput Annex.QuietOutput))
		"avoid verbose output"
	, Option ['v'] ["verbose"] (NoArg (setoutput Annex.NormalOutput))
		"allow verbose output (default)"
	, Option ['j'] ["json"] (NoArg (setoutput Annex.JSONOutput))
		"enable JSON output"
	, Option ['d'] ["debug"] (NoArg (setdebug))
		"show debug messages"
	, Option ['b'] ["backend"] (ReqArg setforcebackend paramName)
		"specify key-value backend to use"
	]
	where
		setforce v = Annex.changeState $ \s -> s { Annex.force = v }
		setfast v = Annex.changeState $ \s -> s { Annex.fast = v }
		setauto v = Annex.changeState $ \s -> s { Annex.auto = v }
		setoutput v = Annex.changeState $ \s -> s { Annex.output = v }
		setforcebackend v = Annex.changeState $ \s -> s { Annex.forcebackend = Just v }
		setdebug = liftIO $ updateGlobalLogger rootLoggerName $
			setLevel DEBUG
	
matcherOptions :: [Option]
matcherOptions =
	[ longopt "not" "negate next option"
	, longopt "and" "both previous and next option must match"
	, longopt "or" "either previous or next option must match"
	, shortopt "(" "open group of options"
	, shortopt ")" "close group of options"
	]
	where
		longopt o = Option [] [o] $ NoArg $ addToken o
		shortopt o = Option o [] $ NoArg $ addToken o

{- Descriptions of params used in usage messages. -}
paramPaths :: String
paramPaths = paramOptional $ paramRepeating paramPath -- most often used
paramPath :: String
paramPath = "PATH"
paramKey :: String
paramKey = "KEY"
paramDesc :: String
paramDesc = "DESC"
paramUrl :: String
paramUrl = "URL"
paramNumber :: String
paramNumber = "NUMBER"
paramRemote :: String
paramRemote = "REMOTE"
paramGlob :: String
paramGlob = "GLOB"
paramName :: String
paramName = "NAME"
paramUUID :: String
paramUUID = "UUID"
paramType :: String
paramType = "TYPE"
paramFormat :: String
paramFormat = "FORMAT"
paramKeyValue :: String
paramKeyValue = "K=V"
paramNothing :: String
paramNothing = ""
paramRepeating :: String -> String
paramRepeating s = s ++ " ..."
paramOptional :: String -> String
paramOptional s = "[" ++ s ++ "]"
paramPair :: String -> String -> String
paramPair a b = a ++ " " ++ b
