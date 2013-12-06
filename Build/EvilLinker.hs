{- Allows linking haskell programs too big for all the files to fit in a
 - command line.
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
	many precollectenvline
	env <- collectenvline
	try $ char ' '
	path <- manyTill anyChar (try $ string collectcmd)
	char ' '
	collect2params <- restOfLine
	return $ CmdParams (path ++ collectcmd) (escapeDosPaths collect2params)
		(Just [(collectenv, env)])
  where
  	collectcmd = "collect2.exe"
  	collectenv = "COLLECT_GCC_OPTIONS"
  	collectenvline = do
		string collectenv
		char '='
		restOfLine
 	precollectenvline = do
		notFollowedBy collectenvline
		restOfLine
	
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

{- Find where collect2 calls ld. -}
parseCollect2 :: Parser CmdParams
parseCollect2 = error "TODO"

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

{- Intentionally ignores command failure; the whole point is to work around
 - that. -}
getOutput :: String -> [String] -> Maybe [(String, String)] -> IO String
getOutput cmd params env = do
	putStrLn $ unwords [cmd, show params]
	(log, _ok) <- processTranscript' cmd params env Nothing
	return log

runParser' :: Parser a -> String -> a
runParser' p s = either (error . show) id (parse p "" s)

atFile :: FilePath -> String
atFile f = '@':f

runAtFile :: Parser CmdParams -> String -> FilePath -> [String] -> IO String
runAtFile p s f extraparams = do
	writeFile f (opts c)
	out <- getOutput (cmd c) (atFile f:extraparams) (env c)
	removeFile f
	return out
  where
 	c = runParser' p s

main = do
	ghcout <- getOutput "cabal"
		["build", "--ghc-options=-v -keep-tmp-files"] Nothing
	gccout <- runAtFile parseGhcLink ghcout "gcc.opt" ["-v"]
	writeFile "gcc.out" gccout
	collect2out <- runAtFile parseGccLink gccout "collect2.opt" ["-v"]
	writeFile "collect2.out" collect2out
	void $ runAtFile parseCollect2 collect2out "ld.opt" []
