{- Build man pages.
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Build.Mans where

import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import System.Process
import System.Exit
import Data.Maybe
import Utility.Exception

main :: IO ()
main = do
	mans <- buildMans
	when (any isNothing mans) $
		error "mdwn2man failed"

buildMans :: IO [Maybe FilePath]
buildMans = do
	mansrc <- filter isManSrc <$> getDirectoryContents "doc"
	createDirectoryIfMissing False "man"
	forM mansrc $ \f -> do
		let src = "doc" </> f
		let dest = srcToDest src
		srcm <- getModificationTime src
		destm <- catchMaybeIO $ getModificationTime dest
		if (Just srcm > destm)
			then do
				r <- system $ unwords
					[ "./Build/mdwn2man"
					, progName src
					, "1"
					, src
					, "> " ++ dest
					]
				if r == ExitSuccess
					then return (Just dest)
					else return Nothing
			else return (Just dest)

isManSrc :: FilePath -> Bool
isManSrc s = "git-annex" `isPrefixOf` (takeFileName s)
	&& takeExtension s == ".mdwn"

srcToDest :: FilePath -> FilePath
srcToDest s = "man" </> progName s ++ ".1"

progName :: FilePath -> FilePath
progName = dropExtension . takeFileName
