{- git fsck interface
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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

import qualified Data.Set as S
import Control.Concurrent.Async
import qualified Data.Semigroup as Sem
import Prelude

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

appendFsckOutput :: FsckOutput -> FsckOutput -> FsckOutput
appendFsckOutput (FsckOutput s1 t1) (FsckOutput s2 t2) =
	FsckOutput (S.union s1 s2) (t1 || t2)
appendFsckOutput (FsckOutput s t) _ = FsckOutput s t
appendFsckOutput _ (FsckOutput s t) = FsckOutput s t
appendFsckOutput NoFsckOutput NoFsckOutput = NoFsckOutput
appendFsckOutput AllDuplicateEntriesWarning AllDuplicateEntriesWarning = AllDuplicateEntriesWarning
appendFsckOutput AllDuplicateEntriesWarning NoFsckOutput = AllDuplicateEntriesWarning
appendFsckOutput NoFsckOutput AllDuplicateEntriesWarning = AllDuplicateEntriesWarning

instance Sem.Semigroup FsckOutput where
	(<>) = appendFsckOutput

instance Monoid FsckOutput where
	mempty = NoFsckOutput

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
	let (command, params) = ("git", fsckParams r)
	(command', params') <- if batchmode
		then toBatchCommand (command, params)
		else return (command, params)
	
	let p = (proc command' (toCommand params'))
		{ std_out = CreatePipe
		, std_err = CreatePipe
		}
	withCreateProcess p go
  where
	go _ (Just outh) (Just errh) pid = do
		(o1, o2) <- concurrently
			(parseFsckOutput maxobjs r outh pid)
			(parseFsckOutput maxobjs r errh pid)
		fsckok <- checkSuccessProcess pid
		case mappend o1 o2 of
			FsckOutput badobjs truncated
				| S.null badobjs && not fsckok -> return FsckFailed
				| otherwise -> return $ FsckFoundMissing badobjs truncated
			NoFsckOutput
				| not fsckok -> return FsckFailed
				| otherwise -> return noproblem
			-- If all fsck output was duplicateEntries warnings,
			-- the repository is not broken, it just has some
			-- unusual tree objects in it. So ignore nonzero
			-- exit status.
			AllDuplicateEntriesWarning -> return noproblem
	go _ _ _ _ = error "internal"
	
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

parseFsckOutput :: Int -> Repo -> Handle -> ProcessHandle -> IO FsckOutput
parseFsckOutput maxobjs r h pid = do
	ls <- getlines []
	if null ls
		then return NoFsckOutput
		else if all ("duplicateEntries" `isInfixOf`) ls
			then return AllDuplicateEntriesWarning
			else do
				let shas = findShas ls
				let !truncated = length shas > maxobjs
				missingobjs <- findMissing (take maxobjs shas) r
				return $ FsckOutput missingobjs truncated
  where
	getlines c = hGetLineUntilExitOrEOF pid h >>= \case
		Nothing -> return (reverse c)
		Just l -> getlines (l:c)

isMissing :: Sha -> Repo -> IO Bool
isMissing s r = either (const True) (const False) <$> tryIO dump
  where
	dump = runQuiet
		[ Param "show"
		, Param (fromRef s)
		] r

findShas :: [String] -> [Sha]
findShas = catMaybes . map (extractSha . encodeBS') 
	. concat . map words . filter wanted
  where
	wanted l = not ("dangling " `isPrefixOf` l)

fsckParams :: Repo -> [CommandParam]
fsckParams = gitCommandLine $ map Param
	[ "fsck"
	, "--no-dangling"
	, "--no-reflogs"
	]
