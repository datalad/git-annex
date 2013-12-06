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

data CmdParams = CmdParams { cmd :: String, opts :: String }
	deriving (Show)

{- Find where ghc calls gcc to link the executable. -}
parseGhcLink :: Parser CmdParams
parseGhcLink = do
	many prelinkline
	linkheaderline
	char '"'
	gcccmd <- many1 (noneOf "\"")
	string "\" "
	gccparams <- restOfLine
	return $ CmdParams gcccmd (manglepaths gccparams)
  where
	linkheaderline = do
		string "*** Linker"
		restOfLine
	prelinkline = do
		notFollowedBy linkheaderline
		restOfLine
	manglepaths = replace "\\" "/"

{- Find where gcc calls collect1. -}
parseGccLink :: Parser CmdParams
parseGccLink = do
	many prelinkline
	error "TODO"
  where
 	prelinkline = error "TODO"
	
{- Find where collect1 calls ld. -}
parseCollect1 :: Parser CmdParams
parseCollect1 = error "TODO"

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

{- Intentionally ignores command failure; the whole point is to work around
 - that. -}
getOutput :: String -> [String] -> IO String
getOutput cmd params = do
	putStrLn $ unwords [cmd, show params]
	(log, _ok) <- processTranscript cmd params Nothing
	return log

runParser' :: Parser a -> String -> a
runParser' p s = either (error . show) id (parse p "" s)

atFile :: FilePath -> String
atFile f = '@':f

runAtFile :: Parser CmdParams -> String -> FilePath -> [String] -> IO String
runAtFile p s f extraparams = do
	writeFile f (opts c)
	out <- getOutput (cmd c) (atFile f:extraparams)
	removeFile f
	return out
  where
 	c = runParser' p s

main = do
	ghcout <- getOutput "cabal"
		["build", "--ghc-options=-v -keep-tmp-files"]
	gccout <- runAtFile parseGhcLink ghcout "gcc.opt" ["-v"]
	writeFile "gcc.out" gccout
	collect1out <- runAtFile parseGccLink gccout "collect1.opt" ["-v"]
	writeFile "collect1.out" collect1out
	void $ runAtFile parseCollect1 collect1out "ld.opt" []
