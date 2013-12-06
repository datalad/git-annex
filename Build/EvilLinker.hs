{- Allows linking haskell programs too big for all the files to fit in a
 - command line.
 -
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import Data.Maybe
import Data.Either
import Data.List
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
	many prelinklines
	linkheaderline
	char '"'
	gcccmd <- many1 (noneOf "\"")
	string "\" "
	gccparams <- restOfLine
	return $ CmdParams gcccmd gccparams
  where
	linkheaderline = do
		string "*** Linker"
		restOfLine
	prelinklines = do
		notFollowedBy linkheaderline
		restOfLine

{- Find where gcc calls collect1. -}
parseCollect1 :: Parser CmdParams
parseCollect1 = error "TODO"
	
{- Find where collect1 calls ld. -}
parseLd :: Parser CmdParams
parseLd = error "TODO"

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

getOutput :: String -> [String] -> IO String
getOutput cmd params = do
	putStrLn $ unwords [cmd, show params]
	(log, ok) <- processTranscript cmd params Nothing
	unless ok $
		error $ cmd ++ " failed:\n\n" ++ log
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
	collect1out <- runAtFile parseCollect1 gccout "collect1.opt" ["-v"]
	writeFile "collect1.out" collect1out
	void $ runAtFile parseLd collect1out "ld.opt" []
