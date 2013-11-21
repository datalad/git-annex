{- git fsck interface
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Fsck (
	FsckResults,
	MissingObjects,
	findBroken,
	foundBroken,
	findMissing,
) where

import Common
import Git
import Git.Command
import Git.Sha
import Utility.Batch

import qualified Data.Set as S

type MissingObjects = S.Set Sha

{- If fsck succeeded, Just a set of missing objects it found.
 - If it failed, Nothing. -}
type FsckResults = Maybe MissingObjects

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
	(output, fsckok) <- processTranscript command' (toCommand params') Nothing
	let objs = findShas output
	badobjs <- findMissing objs r
	if S.null badobjs && not fsckok
		then return Nothing
		else return $ Just badobjs
  where
	(command, params) = ("git", fsckParams r)
	(command', params')
		| batchmode = toBatchCommand (command, params)
		| otherwise = (command, params)

foundBroken :: FsckResults -> Bool
foundBroken Nothing = True
foundBroken (Just s) = not (S.null s)

{- Finds objects that are missing from the git repsitory, or are corrupt.
 -
 - This does not use git cat-file --batch, because catting a corrupt
 - object can cause it to crash, or to report incorrect size information.
 -}
findMissing :: [Sha] -> Repo -> IO MissingObjects
findMissing objs r = S.fromList <$> filterM (not <$$> cancat) objs
  where
	cancat o = either (const False) (const True) <$> tryIO (cat o)
	cat o = runQuiet
		[ Param "cat-file"
		, Param "-p"
		, Param (show o)
		] r

findShas :: String -> [Sha]
findShas = catMaybes . map extractSha . concat . map words . lines

fsckParams :: Repo -> [CommandParam]
fsckParams = gitCommandLine $
	[ Param "fsck"
	, Param "--no-dangling"
	, Param "--no-reflogs"
	]
