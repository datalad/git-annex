{- git-annex output messages
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages (
	showStart,
	showNote,
	showAction,
	showProgress,
	showSideAction,
	showOutput,
	showLongNote,
	showEndOk,
	showEndFail,
	showEndResult,
	showErr,
	warning,
	indent,
	maybeShowJSON,
	setupConsole
) where

import Text.JSON

import Common
import Types
import qualified Annex
import qualified Messages.JSON as JSON

showStart :: String -> String -> Annex ()
showStart command file = handle (JSON.start command file) $
	flushed $ putStr $ command ++ " " ++ file ++ " "

showNote :: String -> Annex ()
showNote s = handle (JSON.note s) $
	flushed $ putStr $ "(" ++ s ++ ") "

showAction :: String -> Annex ()
showAction s = showNote $ s ++ "..."

showProgress :: Annex ()
showProgress = handle q $
	flushed $ putStr "."

showSideAction :: String -> Annex ()
showSideAction s = handle q $
	putStrLn $ "(" ++ s ++ "...)"

showOutput :: Annex ()
showOutput = handle q $
	putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = handle (JSON.note s) $
	putStrLn $ '\n' : indent s

showEndOk :: Annex ()
showEndOk = showEndResult True

showEndFail :: Annex ()
showEndFail = showEndResult False

showEndResult :: Bool -> Annex ()
showEndResult b = handle (JSON.end b) $ putStrLn msg
	where
		msg
			| b = "ok"
			| otherwise = "failed"

showErr :: (Show a) => a -> Annex ()
showErr e = warning' $ "git-annex: " ++ show e

warning :: String -> Annex ()
warning w = warning' (indent w)

warning' :: String -> Annex ()
warning' w = do
	handle q $ putStr "\n"
	liftIO $ do
		hFlush stdout
		hPutStrLn stderr w

indent :: String -> String
indent s = join "\n" $ map (\l -> "  " ++ l) $ lines s

{- By default, haskell honors the user's locale in its output to stdout
 - and stderr. While that's great for proper unicode support, for git-annex
 - all that's really needed is the ability to display simple messages
 - (currently untranslated), and importantly, to display filenames exactly
 - as they are written on disk, no matter what their encoding. So, force
 - raw mode. 
 -
 - NB: Once git-annex gets localized, this will need a rethink. -}
setupConsole :: IO ()
setupConsole = do
	hSetBinaryMode stdout True
	hSetBinaryMode stderr True

handle :: IO () -> IO () -> Annex ()
handle json normal = do
	output <- Annex.getState Annex.output
	case output of
		Annex.NormalOutput -> liftIO normal
		Annex.QuietOutput -> q
		Annex.JSONOutput -> liftIO json

{- Shows a JSON value only when in json mode. -}
maybeShowJSON :: JSON a => [(String, a)] -> Annex ()
maybeShowJSON v = handle (JSON.add v) q

q :: Monad m => m ()
q = return ()

flushed :: IO () -> IO ()
flushed a = a >> hFlush stdout
