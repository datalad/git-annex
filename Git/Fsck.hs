{- git fsck interface
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Fsck (
	findBroken,
	findMissing
) where

import Common
import Git
import Git.Command
import Git.Sha
import Git.CatFile

import qualified Data.Set as S

{- Runs fsck to find some of the broken objects in the repository.
 - May not find all broken objects, if fsck fails on bad data in some of
 - the broken objects it does find. If the fsck fails generally without
 - finding any broken objects, returns Nothing.
 -
 - Strategy: Rather than parsing fsck's current specific output,
 - look for anything in its output (both stdout and stderr) that appears
 - to be a git sha. Not all such shas are of broken objects, so ask git
 - to try to cat the object, and see if it fails.
 -}
findBroken :: Repo -> IO (Maybe (S.Set Sha))
findBroken r = do
	(output, fsckok) <- processTranscript "git" (toCommand $ fsckParams r) Nothing
	let objs = parseFsckOutput output
	badobjs <- findMissing objs r
	if S.null badobjs && not fsckok
		then return Nothing
		else return $ Just badobjs

{- Finds objects that are missing from the git repsitory, or are corrupt.
 - Note that catting a corrupt object will cause cat-file to crash. -}
findMissing :: [Sha] -> Repo -> IO (S.Set Sha)
findMissing objs r = go objs [] =<< start
  where
	start = catFileStart' False r
	go [] c h = do
		catFileStop h
		return $ S.fromList c
	go (o:os) c h = do
		v <- tryIO $ isNothing <$> catObjectDetails h o
		case v of
			Left _ -> do
				void $ tryIO $ catFileStop h
				go os (o:c) =<< start
			Right True -> go os (o:c) h
			Right False -> go os c h

parseFsckOutput :: String -> [Sha]
parseFsckOutput = catMaybes . map extractSha . concat . map words . lines

fsckParams :: Repo -> [CommandParam]
fsckParams = gitCommandLine
	[ Param "fsck"
	, Param "--no-dangling"
	, Param "--no-reflogs"
	]
