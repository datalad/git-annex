{- git branch stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Branch where

import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git
import Git.Sha
import Git.Command

{- The currently checked out branch. -}
current :: Repo -> IO (Maybe Git.Ref)
current r = parse <$> pipeRead [Param "symbolic-ref", Param "HEAD"] r
	where
		parse v
			| L.null v = Nothing
			| otherwise = Just $ Git.Ref $ firstLine $ L.unpack v

{- Checks if the second branch has any commits not present on the first
 - branch. -}
changed :: Branch -> Branch -> Repo -> IO Bool
changed origbranch newbranch repo
	| origbranch == newbranch = return False
	| otherwise = not . L.null <$> diffs
	where
		diffs = pipeRead
			[ Param "log"
			, Param (show origbranch ++ ".." ++ show newbranch)
			, Params "--oneline -n1"
			] repo

{- Given a set of refs that are all known to have commits not
 - on the branch, tries to update the branch by a fast-forward.
 -
 - In order for that to be possible, one of the refs must contain
 - every commit present in all the other refs.
 -}
fastForward :: Branch -> [Ref] -> Repo -> IO Bool
fastForward _ [] _ = return True
fastForward branch (first:rest) repo =
	-- First, check that the branch does not contain any
	-- new commits that are not in the first ref. If it does,
	-- cannot fast-forward.
	ifM (changed first branch repo)
		( no_ff
		, maybe no_ff do_ff =<< findbest first rest
		)
	where
		no_ff = return False
		do_ff to = do
			run "update-ref"
				[Param $ show branch, Param $ show to] repo
			return True
		findbest c [] = return $ Just c
		findbest c (r:rs)
			| c == r = findbest c rs
			| otherwise = do
			better <- changed c r repo
			worse <- changed r c repo
			case (better, worse) of
				(True, True) -> return Nothing -- divergent fail
				(True, False) -> findbest r rs -- better
				(False, True) -> findbest c rs -- worse
				(False, False) -> findbest c rs -- same

{- Commits the index into the specified branch (or other ref), 
 - with the specified parent refs, and returns the committed sha -}
commit :: String -> Branch -> [Ref] -> Repo -> IO Sha
commit message branch parentrefs repo = do
	tree <- getSha "write-tree" $ asString $
		pipeRead [Param "write-tree"] repo
	sha <- getSha "commit-tree" $ asString $
		ignorehandle $ pipeWriteRead
			(map Param $ ["commit-tree", show tree] ++ ps)
			(L.pack message) repo
	run "update-ref" [Param $ show branch, Param $ show sha] repo
	return sha
	where
		ignorehandle a = snd <$> a
		asString a = L.unpack <$> a
		ps = concatMap (\r -> ["-p", show r]) parentrefs
