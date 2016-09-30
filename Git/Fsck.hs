{- git fsck interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Fsck (
	FsckResults(..),
	MissingObjects,
	findBroken,
	foundBroken,
	findMissing,
	isMissing,
	knownMissing,
) where

import Common
import Git
import Git.Command
import Git.Sha
import Utility.Batch
import qualified Git.Version

import qualified Data.Set as S
import Control.Concurrent.Async

type MissingObjects = S.Set Sha

data FsckResults 
	= FsckFoundMissing
		{ missingObjects :: MissingObjects
		, missingObjectsTruncated :: Bool
		}
	| FsckFailed
	deriving (Show)

{- Runs fsck to find some of the broken objects in the repository.
 - May not find all broken objects, if fsck fails on bad data in some of
 - the broken objects it does find.
 -
 - Strategy: Rather than parsing fsck's current specific output,
 - look for anything in its output (both stdout and stderr) that appears
 - to be a git sha. Not all such shas are of broken objects, so ask git
 - to try to cat the object, and see if it fails.
 -}
findBroken :: Bool -> Repo -> IO FsckResults
findBroken batchmode r = do
	supportsNoDangling <- (>= Git.Version.normalize "1.7.10")
		<$> Git.Version.installed
	let (command, params) = ("git", fsckParams supportsNoDangling r)
	(command', params') <- if batchmode
		then toBatchCommand (command, params)
		else return (command, params)
	
	p@(_, _, _, pid) <- createProcess $
		(proc command' (toCommand params'))
			{ std_out = CreatePipe
			, std_err = CreatePipe
			}
	(bad1, bad2) <- concurrently
		(readMissingObjs maxobjs r supportsNoDangling (stdoutHandle p))
		(readMissingObjs maxobjs r supportsNoDangling (stderrHandle p))
	fsckok <- checkSuccessProcess pid
	let truncated = S.size bad1 == maxobjs || S.size bad1 == maxobjs
	let badobjs = S.union bad1 bad2

	if S.null badobjs && not fsckok
		then return FsckFailed
		else return $ FsckFoundMissing badobjs truncated
  where
	maxobjs = 10000

foundBroken :: FsckResults -> Bool
foundBroken FsckFailed = True
foundBroken (FsckFoundMissing s _) = not (S.null s)

knownMissing :: FsckResults -> MissingObjects
knownMissing FsckFailed = S.empty
knownMissing (FsckFoundMissing s _) = s

{- Finds objects that are missing from the git repsitory, or are corrupt.
 -
 - This does not use git cat-file --batch, because catting a corrupt
 - object can cause it to crash, or to report incorrect size information.
 -}
findMissing :: [Sha] -> Repo -> IO MissingObjects
findMissing objs r = S.fromList <$> filterM (`isMissing` r) objs

readMissingObjs :: Int -> Repo -> Bool -> Handle -> IO MissingObjects
readMissingObjs maxobjs r supportsNoDangling h = do
	objs <- take maxobjs . findShas supportsNoDangling <$> hGetContents h
	findMissing objs r

isMissing :: Sha -> Repo -> IO Bool
isMissing s r = either (const True) (const False) <$> tryIO dump
  where
	dump = runQuiet
		[ Param "show"
		, Param (fromRef s)
		] r

findShas :: Bool -> String -> [Sha]
findShas supportsNoDangling = catMaybes . map extractSha . concat . map words . filter wanted . lines
  where
	wanted l
		-- Skip lines like "error in tree <sha>: duplicateEntries: contains duplicate file entries"
		| "duplicateEntries" `isPrefixOf` l = False
		| supportsNoDangling = True
		| otherwise = not ("dangling " `isPrefixOf` l)

fsckParams :: Bool -> Repo -> [CommandParam]
fsckParams supportsNoDangling = gitCommandLine $ map Param $ catMaybes
	[ Just "fsck"
	, if supportsNoDangling
		then Just "--no-dangling"
		else Nothing
	, Just "--no-reflogs"
	]
