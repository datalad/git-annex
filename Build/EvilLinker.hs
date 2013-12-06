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

data CmdParams = CmdParams String String
	deriving (Show)

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

main = do
	ghcout <- getOutput "cabal"
		["build", "--ghc-options=-v -keep-tmp-files"]
	print $ runParser' parseGhcLink ghcout
