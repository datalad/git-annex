{- git ref stuff
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Ref where

import Common
import Git
import Git.Command
import Git.Sha
import Git.Types

import Data.Char (chr)

headRef :: Ref
headRef = Ref "HEAD"

headFile :: Repo -> FilePath
headFile r = localGitDir r </> "HEAD"

setHeadRef :: Ref -> Repo -> IO ()
setHeadRef ref r = writeFile (headFile r) ("ref: " ++ fromRef ref)

{- Converts a fully qualified git ref into a user-visible string. -}
describe :: Ref -> String
describe = fromRef . base

{- Often git refs are fully qualified 
 - (eg refs/heads/master or refs/remotes/origin/master).
 - Converts such a fully qualified ref into a base ref
 - (eg: master or origin/master). -}
base :: Ref -> Ref
base = Ref . remove "refs/heads/" . remove "refs/remotes/" . fromRef
  where
	remove prefix s
		| prefix `isPrefixOf` s = drop (length prefix) s
		| otherwise = s

{- Given a directory such as "refs/remotes/origin", and a ref such as
 - refs/heads/master, yields a version of that ref under the directory,
 - such as refs/remotes/origin/master. -}
underBase :: String -> Ref -> Ref
underBase dir r = Ref $ dir ++ "/" ++ fromRef (base r)

{- Convert a branch such as "master" into a fully qualified ref. -}
branchRef :: Branch -> Ref
branchRef = underBase "refs/heads"

{- A Ref that can be used to refer to a file in the repository, as staged
 - in the index.
 -
 - Prefixing the file with ./ makes this work even if in a subdirectory
 - of a repo.
 -}
fileRef :: FilePath -> Ref
fileRef f = Ref $ ":./" ++ f

{- Converts a Ref to refer to the content of the Ref on a given date. -}
dateRef :: Ref -> RefDate -> Ref
dateRef (Ref r) (RefDate d) = Ref $ r ++ "@" ++ d

{- A Ref that can be used to refer to a file in the repository as it
 - appears in a given Ref. -}
fileFromRef :: Ref -> FilePath -> Ref
fileFromRef (Ref r) f = let (Ref fr) = fileRef f in Ref (r ++ fr)

{- Checks if a ref exists. -}
exists :: Ref -> Repo -> IO Bool
exists ref = runBool
	[Param "show-ref", Param "--verify", Param "-q", Param $ fromRef ref]

{- The file used to record a ref. (Git also stores some refs in a
 - packed-refs file.) -}
file :: Ref -> Repo -> FilePath
file ref repo = localGitDir repo </> fromRef ref

{- Checks if HEAD exists. It generally will, except for in a repository
 - that was just created. -}
headExists :: Repo -> IO Bool
headExists repo = do
	ls <- lines <$> pipeReadStrict [Param "show-ref", Param "--head"] repo
	return $ any (" HEAD" `isSuffixOf`) ls

{- Get the sha of a fully qualified git ref, if it exists. -}
sha :: Branch -> Repo -> IO (Maybe Sha)
sha branch repo = process <$> showref repo
  where
	showref = pipeReadStrict [Param "show-ref",
		Param "--hash", -- get the hash
		Param $ fromRef branch]
	process [] = Nothing
	process s = Just $ Ref $ firstLine s

headSha :: Repo -> IO (Maybe Sha)
headSha = sha headRef

{- List of (shas, branches) matching a given ref or refs. -}
matching :: [Ref] -> Repo -> IO [(Sha, Branch)]
matching refs repo =  matching' (map fromRef refs) repo

{- Includes HEAD in the output, if asked for it. -}
matchingWithHEAD :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingWithHEAD refs repo = matching' ("--head" : map fromRef refs) repo

{- List of (shas, branches) matching a given ref spec. -}
matching' :: [String] -> Repo -> IO [(Sha, Branch)]
matching' ps repo = map gen . lines <$> 
	pipeReadStrict (Param "show-ref" : map Param ps) repo
  where
	gen l = let (r, b) = separate (== ' ') l
		in (Ref r, Ref b)

{- List of (shas, branches) matching a given ref.
 - Duplicate shas are filtered out. -}
matchingUniq :: [Ref] -> Repo -> IO [(Sha, Branch)]
matchingUniq refs repo = nubBy uniqref <$> matching refs repo
  where
	uniqref (a, _) (b, _) = a == b

{- List of all refs. -}
list :: Repo -> IO [(Sha, Ref)]
list = matching' []

{- Deletes a ref. This can delete refs that are not branches, 
 - which git branch --delete refuses to delete. -}
delete :: Sha -> Ref -> Repo -> IO ()
delete oldvalue ref = run
	[ Param "update-ref"
	, Param "-d"
	, Param $ fromRef ref
	, Param $ fromRef oldvalue
	]

{- Gets the sha of the tree a ref uses. -}
tree :: Ref -> Repo -> IO (Maybe Sha)
tree (Ref ref) = extractSha <$$> pipeReadStrict
	[ Param "rev-parse", Param ref' ]
  where
	ref' = if ":" `isInfixOf` ref
		then ref
		-- de-reference commit objects to the tree
		else ref ++ ":"

{- Checks if a String is a legal git ref name.
 -
 - The rules for this are complex; see git-check-ref-format(1) -}
legal :: Bool -> String -> Bool
legal allowonelevel s = all (== False) illegal
  where
	illegal =
		[ any ("." `isPrefixOf`) pathbits
		, any (".lock" `isSuffixOf`) pathbits
		, not allowonelevel && length pathbits < 2
		, contains ".."
		, any (\c -> contains [c]) illegalchars
		, begins "/"
		, ends "/"
		, contains "//"
		, ends "."
		, contains "@{"
		, null s
		]
	contains v = v `isInfixOf` s
	ends v = v `isSuffixOf` s
	begins v = v `isPrefixOf` s

	pathbits = splitc '/' s
	illegalchars = " ~^:?*[\\" ++ controlchars
	controlchars = chr 0o177 : [chr 0 .. chr (0o40-1)]
