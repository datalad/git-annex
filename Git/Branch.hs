{- git branch stuff
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.Branch where

import Common
import Git
import Git.Sha
import Git.Command
import qualified Git.Ref

{- The currently checked out branch.
 -
 - In a just initialized git repo before the first commit,
 - symbolic-ref will show the master branch, even though that
 - branch is not created yet. So, this also looks at show-ref HEAD
 - to double-check.
 -}
current :: Repo -> IO (Maybe Git.Ref)
current r = do
	v <- currentUnsafe r
	case v of
		Nothing -> return Nothing
		Just branch -> 
			ifM (null <$> pipeReadStrict [Param "show-ref", Param $ show branch] r)
				( return Nothing
				, return v
				)

{- The current branch, which may not really exist yet. -}
currentUnsafe :: Repo -> IO (Maybe Git.Ref)
currentUnsafe r = parse . firstLine
	<$> pipeReadStrict [Param "symbolic-ref", Param $ show Git.Ref.headRef] r
  where
	parse l
		| null l = Nothing
		| otherwise = Just $ Git.Ref l

{- Checks if the second branch has any commits not present on the first
 - branch. -}
changed :: Branch -> Branch -> Repo -> IO Bool
changed origbranch newbranch repo
	| origbranch == newbranch = return False
	| otherwise = not . null <$> diffs
  where
	diffs = pipeReadStrict
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
		run [Param "update-ref", Param $ show branch, Param $ show to] repo
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
	tree <- getSha "write-tree" $
		pipeReadStrict [Param "write-tree"] repo
	sha <- getSha "commit-tree" $ pipeWriteRead
		(map Param $ ["commit-tree", show tree] ++ ps)
		(Just $ flip hPutStr message) repo
	update branch sha repo
	return sha
  where
	ps = concatMap (\r -> ["-p", show r]) parentrefs

{- A leading + makes git-push force pushing a branch. -}
forcePush :: String -> String
forcePush b = "+" ++ b

{- Updates a branch (or other ref) to a new Sha. -}
update :: Branch -> Sha -> Repo -> IO ()
update branch sha = run 
	[ Param "update-ref"
	, Param $ show branch
	, Param $ show sha
	]

{- Checks out a branch, creating it if necessary. -}
checkout :: Branch -> Repo -> IO ()
checkout branch = run
	[ Param "checkout"
	, Param "-q"
	, Param "-B"
	, Param $ show $ Git.Ref.base branch
	]

{- Removes a branch. -}
delete :: Branch -> Repo -> IO ()
delete branch = run
	[ Param "branch"
	, Param "-q"
	, Param "-D"
	, Param $ show $ Git.Ref.base branch
	]
