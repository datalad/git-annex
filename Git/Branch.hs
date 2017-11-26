{- git branch stuff
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.Branch where

import Common
import Git
import Git.Sha
import Git.Command
import qualified Git.Config
import qualified Git.Ref
import qualified Git.BuildVersion

{- The currently checked out branch.
 -
 - In a just initialized git repo before the first commit,
 - symbolic-ref will show the master branch, even though that
 - branch is not created yet. So, this also looks at show-ref HEAD
 - to double-check.
 -}
current :: Repo -> IO (Maybe Branch)
current r = do
	v <- currentUnsafe r
	case v of
		Nothing -> return Nothing
		Just branch -> 
			ifM (null <$> pipeReadStrict [Param "show-ref", Param $ fromRef branch] r)
				( return Nothing
				, return v
				)

{- The current branch, which may not really exist yet. -}
currentUnsafe :: Repo -> IO (Maybe Branch)
currentUnsafe r = parse . firstLine
	<$> pipeReadStrict [Param "symbolic-ref", Param "-q", Param $ fromRef Git.Ref.headRef] r
  where
	parse l
		| null l = Nothing
		| otherwise = Just $ Git.Ref l

{- Checks if the second branch has any commits not present on the first
 - branch. -}
changed :: Branch -> Branch -> Repo -> IO Bool
changed origbranch newbranch repo
	| origbranch == newbranch = return False
	| otherwise = not . null
		<$> changed' origbranch newbranch [Param "-n1"] repo
  where

changed' :: Branch -> Branch -> [CommandParam] -> Repo -> IO String
changed' origbranch newbranch extraps repo = pipeReadStrict ps repo
  where
	ps =
		[ Param "log"
		, Param (fromRef origbranch ++ ".." ++ fromRef newbranch)
		, Param "--pretty=%H"
		] ++ extraps

{- Lists commits that are in the second branch and not in the first branch. -}
changedCommits :: Branch -> Branch -> [CommandParam] -> Repo -> IO [Sha]
changedCommits origbranch newbranch extraps repo = 
	catMaybes . map extractSha . lines
		<$> changed' origbranch newbranch extraps repo
	
{- Check if it's possible to fast-forward from the old
 - ref to the new ref.
 -
 - This requires there to be a path from the old to the new. -}
fastForwardable :: Ref -> Ref -> Repo -> IO Bool
fastForwardable old new repo = not . null <$>
	pipeReadStrict
		[ Param "log"
		, Param $ fromRef old ++ ".." ++ fromRef new
		, Param "-n1"
		, Param "--pretty=%H"
		, Param "--ancestry-path"
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
		update' branch to repo
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

{- The user may have set commit.gpgsign, intending all their manual
 - commits to be signed. But signing automatic/background commits could
 - easily lead to unwanted gpg prompts or failures.
 -}
data CommitMode = ManualCommit | AutomaticCommit
	deriving (Eq)

{- Prevent signing automatic commits. -}
applyCommitMode :: CommitMode -> [CommandParam] -> [CommandParam]
applyCommitMode commitmode ps
	| commitmode == AutomaticCommit && not (Git.BuildVersion.older "2.0.0") =
		Param "--no-gpg-sign" : ps
	| otherwise = ps

{- Some versions of git commit-tree honor commit.gpgsign themselves,
 - but others need -S to be passed to enable gpg signing of manual commits. -}
applyCommitModeForCommitTree :: CommitMode -> [CommandParam] -> Repo -> [CommandParam]
applyCommitModeForCommitTree commitmode ps r
	| commitmode == ManualCommit =
		case (Git.Config.getMaybe "commit.gpgsign" r) of
			Just s | Git.Config.isTrue s == Just True ->
				Param "-S":ps
			_ -> ps'
	| otherwise = ps'
  where
	ps' = applyCommitMode commitmode ps

{- Commit via the usual git command. -}
commitCommand :: CommitMode -> [CommandParam] -> Repo -> IO Bool
commitCommand = commitCommand' runBool

commitCommand' :: ([CommandParam] -> Repo -> IO a) -> CommitMode -> [CommandParam] -> Repo -> IO a
commitCommand' runner commitmode ps = runner $
	Param "commit" : applyCommitMode commitmode ps

{- Commits the index into the specified branch (or other ref), 
 - with the specified parent refs, and returns the committed sha.
 -
 - Without allowempy set, avoids making a commit if there is exactly
 - one parent, and it has the same tree that would be committed.
 -
 - Unlike git-commit, does not run any hooks, or examine the work tree
 - in any way.
 -}
commit :: CommitMode -> Bool -> String -> Branch -> [Ref] -> Repo -> IO (Maybe Sha)
commit commitmode allowempty message branch parentrefs repo = do
	tree <- getSha "write-tree" $
		pipeReadStrict [Param "write-tree"] repo
	ifM (cancommit tree)
		( do
			sha <- commitTree commitmode message parentrefs tree repo
			update' branch sha repo
			return $ Just sha
		, return Nothing
		)
  where
	cancommit tree
		| allowempty = return True
		| otherwise = case parentrefs of
			[p] -> maybe False (tree /=) <$> Git.Ref.tree p repo
			_ -> return True

commitAlways :: CommitMode -> String -> Branch -> [Ref] -> Repo -> IO Sha
commitAlways commitmode message branch parentrefs repo = fromJust
	<$> commit commitmode True message branch parentrefs repo

commitTree :: CommitMode -> String -> [Ref] -> Ref -> Repo -> IO Sha
commitTree commitmode message parentrefs tree repo =
	getSha "commit-tree" $
		pipeWriteRead ([Param "commit-tree", Param (fromRef tree)] ++ ps)
			sendmsg repo
  where
	sendmsg = Just $ flip hPutStr message
	ps = applyCommitModeForCommitTree commitmode parentparams repo
	parentparams = map Param $ concatMap (\r -> ["-p", fromRef r]) parentrefs

{- A leading + makes git-push force pushing a branch. -}
forcePush :: String -> String
forcePush b = "+" ++ b

{- Updates a branch (or other ref) to a new Sha or branch Ref. -}
update :: String -> Branch -> Ref -> Repo -> IO ()
update message branch r = run
	[ Param "update-ref"
	, Param "-m"
	, Param message
	, Param $ fromRef branch
	, Param $ fromRef r
	]

update' :: Branch -> Ref -> Repo -> IO ()
update' branch r = run
	[ Param "update-ref"
	, Param $ fromRef branch
	, Param $ fromRef r
	]

{- Checks out a branch, creating it if necessary. -}
checkout :: Branch -> Repo -> IO ()
checkout branch = run
	[ Param "checkout"
	, Param "-q"
	, Param "-B"
	, Param $ fromRef $ Git.Ref.base branch
	]

{- Removes a branch. -}
delete :: Branch -> Repo -> IO ()
delete branch = run
	[ Param "branch"
	, Param "-q"
	, Param "-D"
	, Param $ fromRef $ Git.Ref.base branch
	]
