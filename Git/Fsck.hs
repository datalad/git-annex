{- git fsck interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

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

data FsckResults 
	= FsckFoundMissing
		{ missingObjects :: MissingObjects
		, missingObjectsTruncated :: Bool
		}
	| FsckFailed
	deriving (Show)

data FsckOutput 
	= FsckOutput MissingObjects Truncated
	| NoFsckOutput
	| AllDuplicateEntriesWarning

type MissingObjects = S.Set Sha

type Truncated = Bool

instance Monoid FsckOutput where
	mempty = NoFsckOutput
	mappend (FsckOutput s1 t1) (FsckOutput s2 t2) = FsckOutput (S.union s1 s2) (t1 || t2)
	mappend (FsckOutput s t) _ = FsckOutput s t
	mappend _ (FsckOutput s t) = FsckOutput s t
	mappend NoFsckOutput NoFsckOutput = NoFsckOutput
	mappend AllDuplicateEntriesWarning AllDuplicateEntriesWarning = AllDuplicateEntriesWarning
	mappend AllDuplicateEntriesWarning NoFsckOutput = AllDuplicateEntriesWarning
	mappend NoFsckOutput AllDuplicateEntriesWarning = AllDuplicateEntriesWarning

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
	(o1, o2) <- concurrently
		(parseFsckOutput maxobjs r supportsNoDangling (stdoutHandle p))
		(parseFsckOutput maxobjs r supportsNoDangling (stderrHandle p))
	fsckok <- checkSuccessProcess pid
	case mappend o1 o2 of
		FsckOutput badobjs truncated
			| S.null badobjs && not fsckok -> return FsckFailed
			| otherwise -> return $ FsckFoundMissing badobjs truncated
		NoFsckOutput
			| not fsckok -> return FsckFailed
			| otherwise -> return noproblem
		-- If all fsck output was duplicateEntries warnings,
		-- the repository is not broken, it just has some unusual
		-- tree objects in it. So ignore nonzero exit status.
		AllDuplicateEntriesWarning -> return noproblem
  where
	maxobjs = 10000
	noproblem = FsckFoundMissing S.empty False

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

parseFsckOutput :: Int -> Repo -> Bool -> Handle -> IO FsckOutput
parseFsckOutput maxobjs r supportsNoDangling h = do
	ls <- lines <$> hGetContents h
	if null ls
		then return NoFsckOutput
		else if all ("duplicateEntries" `isInfixOf`) ls
			then return AllDuplicateEntriesWarning
			else do
				let shas = findShas supportsNoDangling ls
				let !truncated = length shas > maxobjs
				missingobjs <- findMissing (take maxobjs shas) r
				return $ FsckOutput missingobjs truncated

isMissing :: Sha -> Repo -> IO Bool
isMissing s r = either (const True) (const False) <$> tryIO dump
  where
	dump = runQuiet
		[ Param "show"
		, Param (fromRef s)
		] r

findShas :: Bool -> [String] -> [Sha]
findShas supportsNoDangling = catMaybes . map extractSha . concat . map words . filter wanted
  where
	wanted l
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
