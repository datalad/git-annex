{- git-annex output messages
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Messages where

import Control.Monad.State (liftIO)
import System.IO
import Control.Monad (unless)
import Data.String.Utils

import Types
import qualified Annex

verbose :: Annex () -> Annex ()
verbose a = do
	q <- Annex.getState Annex.quiet
	unless q a

showStart :: String -> String -> Annex ()
showStart command file = verbose $ liftIO $ do
	putStr $ command ++ " " ++ file ++ " "
	hFlush stdout

showNote :: String -> Annex ()
showNote s = verbose $ liftIO $ do
	putStr $ "(" ++ s ++ ") "
	hFlush stdout

showAction :: String -> Annex ()
showAction s = showNote $ s ++ "..."

showProgress :: Annex ()
showProgress = verbose $ liftIO $ do
	putStr "."
	hFlush stdout

showSideAction :: String -> Annex ()
showSideAction s = verbose $ liftIO $ putStrLn $ "(" ++ s ++ "...)"

showOutput :: Annex ()
showOutput = verbose $ liftIO $ putStr "\n"

showLongNote :: String -> Annex ()
showLongNote s = verbose $ liftIO $ putStr $ '\n' : indent s

showEndOk :: Annex ()
showEndOk = verbose $ liftIO $ putStrLn "ok"

showEndFail :: Annex ()
showEndFail = verbose $ liftIO $ putStrLn "failed"

showEndResult :: Bool -> Annex ()
showEndResult True = showEndOk
showEndResult False = showEndFail

showErr :: (Show a) => a -> Annex ()
showErr e = liftIO $ do
	hFlush stdout
	hPutStrLn stderr $ "git-annex: " ++ show e

warning :: String -> Annex ()
warning w = do
	verbose $ liftIO $ putStr "\n"
	liftIO $ do
		hFlush stdout
		hPutStrLn stderr $ indent w

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
