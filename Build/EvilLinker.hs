{- Allows linking haskell programs too big for all the files to fit in a
 - command line.
 -
 - See https://ghc.haskell.org/trac/ghc/ticket/8596
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import Data.Maybe
import Data.Either
import Data.List
import Data.List.Utils
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))
import Control.Monad

import Utility.Monad
import Utility.Process
import System.Directory

data CmdParams = CmdParams
	{ cmd :: String
	, opts :: String
	, env :: Maybe [(String, String)]
	} deriving (Show)

{- Find where ghc calls gcc to link the executable. -}
parseGhcLink :: Parser CmdParams
parseGhcLink = do
	many prelinkline
	linkheaderline
	char '"'
	gcccmd <- many1 (noneOf "\"")
	string "\" "
	gccparams <- restOfLine
	return $ CmdParams gcccmd (manglepaths gccparams) Nothing
  where
	linkheaderline = do
		string "*** Linker"
		restOfLine
	prelinkline = do
		notFollowedBy linkheaderline
		restOfLine
	manglepaths = replace "\\" "/"

{- Find where gcc calls collect2. -}
parseGccLink :: Parser CmdParams
parseGccLink = do
	many preenv
	env <- collectenv
	try $ char ' '
	path <- manyTill anyChar (try $ string collectcmd)
	char ' '
	collect2params <- restOfLine
	return $ CmdParams (path ++ collectcmd) (escapeDosPaths collect2params) env
  where
  	collectcmd = "collect2.exe"
	pathenv = "COMPILER_PATH"
	libpathenv = "LIBRARY_PATH"
  	optenv = "COLLECT_GCC_OPTIONS"
  	collectenv = do
		string pathenv
		char '='
		p <- restOfLine
		string libpathenv
		char '='
		lp <- restOfLine
		string optenv
		char '='
		o <- restOfLine
		return $ Just [(pathenv, p), (libpathenv, lp), (optenv, o)]
 	preenv = do
		notFollowedBy collectenv
		restOfLine

{- Find where collect2 calls ld. -}
parseCollect2 :: Parser CmdParams
parseCollect2 = do
	string "GNU ld"
	restOfLine
	string "collect2 version"
	restOfLine
	path <- manyTill anyChar (try $ string ldcmd)
	char ' '
	params <- restOfLine
	return $ CmdParams (path ++ ldcmd) (escapeDosPaths params) Nothing
  where
	ldcmd = "ld.exe"
	
{- Input contains something like 
 - c:/program files/haskell platform/foo -LC:/Program Files/Haskell Platform/ -L...
 - and the *right* spaces must be escaped with \
 -
 - Argh.
 -}
escapeDosPaths :: String -> String
escapeDosPaths = replace "Program Files" "Program\\ Files"
	. replace "program files" "program\\ files"
	. replace "Haskell Platform" "Haskell\\ Platform"
	. replace "haskell platform" "haskell\\ platform"
	. replace "Application Data" "Application\\ Data"

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

getOutput :: String -> [String] -> Maybe [(String, String)] -> IO (String, Bool)
getOutput cmd params env = do
	putStrLn $ unwords [cmd, show params]
	processTranscript' cmd params env Nothing

runParser' :: Parser a -> String -> a
runParser' p s = either failedparse id (parse p "" s)
  where
	failedparse e = error $ (show e) ++ "\nInput:\n" ++ s

atFile :: FilePath -> String
atFile f = '@':f

runAtFile :: Parser CmdParams -> String -> FilePath -> [String] -> IO (String, Bool)
runAtFile p s f extraparams = do
	writeFile f (opts c)
	out <- getOutput (cmd c) (atFile f:extraparams) (env c)
	removeFile f
	return out
  where
 	c = runParser' p s

main = do
	ghcout <- fst <$> getOutput "cabal"
		["build", "--ghc-options=-v -keep-tmp-files"] Nothing
	gccout <- fst <$> runAtFile parseGhcLink ghcout "gcc.opt" ["-v"]
	collect2out <- fst <$> runAtFile parseGccLink gccout "collect2.opt" ["-v"]
	writeFile "collect2.out" collect2out
	(out, ok) <- runAtFile parseCollect2 collect2out "ld.opt" []
	unless ok $
		error $ "ld failed:\n" ++ out
