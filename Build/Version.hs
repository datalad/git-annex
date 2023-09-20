{- Package version determination. -}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Build.Version where

import Data.List
import System.Environment
import Data.Char
import System.Process
import Control.Applicative
import Prelude

import Utility.Monad
import Utility.Exception
import Utility.Misc

type Version = String

{- Set when making an official release. (Distribution vendors should set
 - this too.) -}
isReleaseBuild :: IO Bool
isReleaseBuild = (== Just "1") <$> catchMaybeIO (getEnv "RELEASE_BUILD")

{- Version comes from the CHANGELOG, plus the git rev of the last commit.
 - This works for autobuilds, ad-hoc builds, etc.
 -
 - If git or a git repo is not available, or something goes wrong,
 - or this is a release build, just use the version from the CHANGELOG. -}
getVersion :: IO Version
getVersion = do
	changelogversion <- getChangelogVersion
	ifM (isReleaseBuild)
		( return changelogversion
		, catchDefaultIO changelogversion $ do
			gitversion <- takeWhile (\c -> isAlphaNum c) <$> readProcess "sh"
				[ "-c"
				, "git log -n 1 --format=format:'%H'"
				] ""
			return $ if null gitversion
				then changelogversion
				else concat
					[ changelogversion
					, "-g"
					, gitversion
					]
		)
	
getChangelogVersion :: IO Version
getChangelogVersion = do
	changelog <- readFile "CHANGELOG"
	let verline = takeWhile (/= '\n') changelog
	return $ middle (words verline !! 1)
  where
	middle = drop 1 . init

writeVersion :: Version -> IO ()
writeVersion ver = catchMaybeIO (readFileStrict f) >>= \case
	Just s | s == body -> return ()
	_ -> writeFile f body
  where
	body = unlines $ concat
		[ header
		, ["packageversion :: String"]
		, ["packageversion = \"" ++ ver ++ "\""]
		, footer
		]
	header = [
		  "{- Automatically generated. -}"
		, ""
		]
	footer = []
	f = "Build/Version"
