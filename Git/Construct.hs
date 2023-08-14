{- Construction of Git Repo objects
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Git.Construct (
	fromCwd,
	fromAbsPath,
	fromPath,
	fromUrl,
	fromUnknown,
	localToUrl,
	remoteNamed,
	remoteNamedFromKey,
	fromRemotes,
	fromRemoteLocation,
	repoAbsPath,
	checkForRepo,
	newFrom,
	adjustGitDirFile,
	isBareRepo,
) where

#ifndef mingw32_HOST_OS
import System.Posix.User
#endif
import qualified Data.Map as M
import Network.URI

import Common
import Git.Types
import Git
import Git.Remote
import Git.FilePath
import qualified Git.Url as Url
import Utility.UserInfo
import Utility.Url.Parse

import qualified Data.ByteString as B
import qualified System.FilePath.ByteString as P

{- Finds the git repository used for the cwd, which may be in a parent
 - directory. -}
fromCwd :: IO (Maybe Repo)
fromCwd = getCurrentDirectory >>= seekUp
  where
	seekUp dir = do
		r <- checkForRepo dir
		case r of
			Nothing -> case upFrom (toRawFilePath dir) of
				Nothing -> return Nothing
				Just d -> seekUp (fromRawFilePath d)
			Just loc -> pure $ Just $ newFrom loc

{- Local Repo constructor, accepts a relative or absolute path. -}
fromPath :: RawFilePath -> IO Repo
fromPath dir
	-- When dir == "foo/.git", git looks for "foo/.git/.git",
	-- and failing that, uses "foo" as the repository.
	| (P.pathSeparator `B.cons` ".git") `B.isSuffixOf` canondir =
		ifM (doesDirectoryExist $ fromRawFilePath dir </> ".git")
			( ret dir
			, ret (P.takeDirectory canondir)
			)
	| otherwise = ifM (doesDirectoryExist (fromRawFilePath dir))
		( checkGitDirFile dir >>= maybe (ret dir) (pure . newFrom)
		-- git falls back to dir.git when dir doesn't
		-- exist, as long as dir didn't end with a
		-- path separator
		, if dir == canondir
			then ret (dir <> ".git")
			else ret dir
		)
  where
	ret = pure . newFrom . LocalUnknown
	canondir = P.dropTrailingPathSeparator dir

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
fromAbsPath :: RawFilePath -> IO Repo
fromAbsPath dir
	| absoluteGitPath dir = fromPath dir
	| otherwise =
		giveup $ "internal error, " ++ show dir ++ " is not absolute"

{- Construct a Repo for a remote's url.
 -
 - Git is somewhat forgiving about urls to repositories, allowing
 - eg spaces that are not normally allowed unescaped in urls. Such
 - characters get escaped.
 -
 - This will always succeed, even if the url cannot be parsed
 - or is invalid, because git can also function despite remotes having
 - such urls, only failing if such a remote is used.
 -}
fromUrl :: String -> IO Repo
fromUrl url
	| not (isURI url) = fromUrl' $ escapeURIString isUnescapedInURI url
	| otherwise = fromUrl' url

fromUrl' :: String -> IO Repo
fromUrl' url
	| "file://" `isPrefixOf` url = case parseURIPortable url of
		Just u -> fromAbsPath $ toRawFilePath $ unEscapeString $ uriPath u
		Nothing -> pure $ newFrom $ UnparseableUrl url
	| otherwise = case parseURIPortable url of
		Just u -> pure $ newFrom $ Url u
		Nothing -> pure $ newFrom $ UnparseableUrl url

{- Creates a repo that has an unknown location. -}
fromUnknown :: Repo
fromUnknown = newFrom Unknown

{- Converts a local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = case (Url.authority reference, Url.scheme reference) of
		(Just auth, Just s) -> 
			let absurl = concat
				[ s
				, "//"
				, auth
				, fromRawFilePath (repoPath r)
				]
			in r { location = Url $ fromJust $ parseURIPortable absurl }
		_ -> r

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
fromRemotes :: Repo -> IO [Repo]
fromRemotes repo = catMaybes <$> mapM construct remotepairs
  where
	filterconfig f = filter f $ M.toList $ config repo
	filterkeys f = filterconfig (\(k,_) -> f k)
	remotepairs = filterkeys isRemoteUrlKey
	construct (k,v) = remoteNamedFromKey k $
		fromRemoteLocation (fromConfigValue v) False repo

{- Sets the name of a remote when constructing the Repo to represent it. -}
remoteNamed :: String -> IO Repo -> IO Repo
remoteNamed n constructor = do
	r <- constructor
	return $ r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
 - "remote.foo.url". -}
remoteNamedFromKey :: ConfigKey -> IO Repo -> IO (Maybe Repo)
remoteNamedFromKey k r = case remoteKeyToRemoteName k of
	Nothing -> pure Nothing
	Just n -> Just <$> remoteNamed n r

{- Constructs a new Repo for one of a Repo's remotes using a given
 - location (ie, an url). 
 -
 - knownurl can be true if the location is known to be an url. This allows
 - urls that don't parse as urls to be used, returning UnparseableUrl.
 - If knownurl is false, the location may still be an url, if it parses as
 - one.
 -}
fromRemoteLocation :: String -> Bool -> Repo -> IO Repo
fromRemoteLocation s knownurl repo = gen $ parseRemoteLocation s knownurl repo
  where
	gen (RemotePath p) = fromRemotePath p repo
	gen (RemoteUrl u) = fromUrl u

{- Constructs a Repo from the path specified in the git remotes of
 - another Repo. -}
fromRemotePath :: FilePath -> Repo -> IO Repo
fromRemotePath dir repo = do
	dir' <- expandTilde dir
	fromPath $ repoPath repo P.</> toRawFilePath dir'

{- Git remotes can have a directory that is specified relative
 - to the user's home directory, or that contains tilde expansions.
 - This converts such a directory to an absolute path.
 - Note that it has to run on the system where the remote is.
 -}
repoAbsPath :: RawFilePath -> IO RawFilePath
repoAbsPath d = do
	d' <- expandTilde (fromRawFilePath d)
	h <- myHomeDir
	return $ toRawFilePath $ h </> d'

expandTilde :: FilePath -> IO FilePath
#ifdef mingw32_HOST_OS
expandTilde = return
#else
expandTilde p = expandt True p
	-- If unable to expand a tilde, eg due to a user not existing,
	-- use the path as given.
	`catchNonAsync` (const (return p))
  where
	expandt _ [] = return ""
	expandt _ ('/':cs) = do
		v <- expandt True cs
		return ('/':v)
	expandt True ('~':'/':cs) = do
		h <- myHomeDir
		return $ h </> cs
	expandt True "~" = myHomeDir
	expandt True ('~':cs) = do
		let (name, rest) = findname "" cs
		u <- getUserEntryForName name
		return $ homeDirectory u </> rest
	expandt _ (c:cs) = do
		v <- expandt False cs
		return (c:v)
	findname n [] = (n, "")
	findname n (c:cs)
		| c == '/' = (n, cs)
		| otherwise = findname (n++[c]) cs
#endif

{- Checks if a git repository exists in a directory. Does not find
 - git repositories in parent directories. -}
checkForRepo :: FilePath -> IO (Maybe RepoLocation)
checkForRepo dir = 
	check isRepo $
		check (checkGitDirFile (toRawFilePath dir)) $
			check (checkdir (isBareRepo dir)) $
				return Nothing
  where
	check test cont = maybe cont (return . Just) =<< test
	checkdir c = ifM c
		( return $ Just $ LocalUnknown $ toRawFilePath dir
		, return Nothing
		)
	isRepo = checkdir $ 
		doesFileExist (dir </> ".git" </> "config")
			<||>
		-- A git-worktree lacks .git/config, but has .git/gitdir.
		-- (Normally the .git is a file, not a symlink, but it can
		-- be converted to a symlink and git will still work;
		-- this handles that case.)
		doesFileExist (dir </>  ".git" </> "gitdir")

isBareRepo :: FilePath -> IO Bool
isBareRepo dir = doesFileExist (dir </> "config")
	<&&> doesDirectoryExist (dir </> "objects")

-- Check for a .git file.
checkGitDirFile :: RawFilePath -> IO (Maybe RepoLocation)
checkGitDirFile dir = adjustGitDirFile' $ Local 
	{ gitdir = dir P.</> ".git"
	, worktree = Just dir
	}

-- git-submodule, git-worktree, and --separate-git-dir
-- make .git be a file pointing to the real git directory.
-- Detect that, and return a RepoLocation with gitdir pointing 
-- to the real git directory.
adjustGitDirFile :: RepoLocation -> IO RepoLocation
adjustGitDirFile loc = fromMaybe loc <$> adjustGitDirFile' loc

adjustGitDirFile' :: RepoLocation -> IO (Maybe RepoLocation)
adjustGitDirFile' loc = do
	let gd = gitdir loc
	c <- firstLine <$> catchDefaultIO "" (readFile (fromRawFilePath gd))
	if gitdirprefix `isPrefixOf` c
		then do
			top <- fromRawFilePath . P.takeDirectory <$> absPath gd
			return $ Just $ loc
				{ gitdir = absPathFrom 
					(toRawFilePath top)
					(toRawFilePath 
						(drop (length gitdirprefix) c))
				}
		else return Nothing
 where
	gitdirprefix = "gitdir: "


newFrom :: RepoLocation -> Repo
newFrom l = Repo
	{ location = l
	, config = M.empty
	, fullconfig = M.empty
	, remoteName = Nothing
	, gitEnv = Nothing
	, gitEnvOverridesGitDir = False
	, gitGlobalOpts = []
	, gitDirSpecifiedExplicitly = False
	}

