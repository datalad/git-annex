{- Allows linking haskell programs too big for all the files to fit in a
 - command line.
 -
 - See https://ghc.haskell.org/trac/ghc/ticket/8596
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Main where

import Data.List.Utils
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>))
import Control.Monad
import Data.Maybe
import Data.List

import Utility.Monad
import Utility.Process hiding (env)
import qualified Utility.Process
import Utility.Env
import Utility.Directory

data CmdParams = CmdParams
	{ cmd :: String
	, opts :: String
	, env :: Maybe [(String, String)]
	} deriving (Show)

{- Find where ghc calls gcc to link the executable. -}
parseGhcLink :: Parser CmdParams
parseGhcLink = do
	void $ many prelinkline
	void linkheaderline
	void $ char '"'
	gcccmd <- many1 (noneOf "\"")
	void $ string "\" "
	gccparams <- restOfLine
	return $ CmdParams gcccmd (manglepaths gccparams) Nothing
  where
	linkheaderline = do
		void $ string "*** Linker"
		restOfLine
	prelinkline = do
		void $ notFollowedBy linkheaderline
		restOfLine
	manglepaths = replace "\\" "/"

{- Find where gcc calls collect2. -}
parseGccLink :: Parser CmdParams
parseGccLink = do
	cenv <- collectenv
	void $ try $ char ' '
	path <- manyTill anyChar (try $ string collectcmd)
	void $ char ' '
	collect2params <- restOfLine
	return $ CmdParams (path ++ collectcmd) (escapeDosPaths collect2params) cenv
  where
	collectcmd = "collect2.exe"
	collectgccenv = "COLLECT_GCC"
	collectltoenv = "COLLECT_LTO_WRAPPER"
	pathenv = "COMPILER_PATH"
	libpathenv = "LIBRARY_PATH"
	optenv = "COLLECT_GCC_OPTIONS"
	collectenv = do
		void $ many1 $ do
			notFollowedBy $ string collectgccenv
			restOfLine
		void $ string collectgccenv
		void $ char '='
		g <- restOfLine
		void $ string collectltoenv
		void $ char '='
		lt <- restOfLine
		void $ many1 $ do
			notFollowedBy $ string pathenv
			restOfLine
		void $ string pathenv
		void $ char '='
		p <- restOfLine
		void $ string libpathenv
		void $ char '='
		lp <- restOfLine
		void $ string optenv
		void $ char '='
		o <- restOfLine
		return $ Just [(collectgccenv, g), (collectltoenv, lt), (pathenv, p), (libpathenv, lp), (optenv, o)]

{- Find where collect2 calls ld. -}
parseCollect2 :: Parser CmdParams
parseCollect2 = do
	void $ manyTill restOfLine (try versionline)
	path <- manyTill anyChar (try $ string ldcmd)
	void $ char ' '
	params <- restOfLine
	return $ CmdParams (path ++ ldcmd) (escapeDosPaths params) Nothing
  where
	ldcmd = "ld.exe"
	versionline = do
		void $ string "collect2 version"
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
	. replace "Application Data" "Application\\ Data"
	. replace "Documents and Settings" "Documents\\ and\\ Settings"
	. replace "Files (x86)" "Files\\ (x86)"
	. replace "files (x86)" "files\\ (x86)"

restOfLine :: Parser String
restOfLine = newline `after` many (noneOf "\n")

getOutput :: String -> [String] -> Maybe [(String, String)] -> IO (String, Bool)
getOutput c ps environ = do
	putStrLn $ unwords [c, show ps]
	systemenviron <- getEnvironment
	let environ' = fromMaybe [] environ ++ systemenviron
	out@(_, ok) <- processTranscript' (\p -> p { Utility.Process.env = Just environ' }) c ps Nothing
	putStrLn $ unwords [c, "finished", show ok]
	return out

atFile :: FilePath -> String
atFile f = '@':f

runAtFile :: Parser CmdParams -> String -> FilePath -> [String] -> IO (String, Bool)
runAtFile p s f extraparams = do
	when (null $ opts c) $
		error $ "failed to find any options for " ++ f ++ " in >>>" ++ s ++ "<<<"
	writeFile f (opts c)
	out <- getOutput (cmd c) (atFile f:extraparams) (env c)
	removeFile f
	return out
  where
	c = case parse p "" s of
		Left e -> error $
			(show e) ++ 
			"\n<<<\n" ++ s ++ "\n>>>"
		Right r -> r

main :: IO ()
main = do
	ghcout <- fst <$> getOutput "cabal"
		["build", "--ghc-options=-v -keep-tmp-files"] Nothing
	gccout <- fst <$> runAtFile parseGhcLink ghcout "gcc.opt" ["-v"]
	collect2out <- fst <$> runAtFile parseGccLink gccout "collect2.opt" ["-v"]
	(out, ok) <- runAtFile parseCollect2 collect2out "ld.opt" []
	unless ok $
		error $ "ld failed:\n" ++ out
