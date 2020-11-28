{- Construction of Git Repo objects
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
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
fromPath dir = fromAbsPath =<< absPath dir

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
fromAbsPath :: RawFilePath -> IO Repo
fromAbsPath dir
	| absoluteGitPath dir = hunt
	| otherwise =
		error $ "internal error, " ++ show dir ++ " is not absolute"
  where
	ret = pure . newFrom . LocalUnknown
	canondir = P.dropTrailingPathSeparator dir
	{- When dir == "foo/.git", git looks for "foo/.git/.git",
	 - and failing that, uses "foo" as the repository. -}
	hunt
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

{- Remote Repo constructor. Throws exception on invalid url.
 -
 - Git is somewhat forgiving about urls to repositories, allowing
 - eg spaces that are not normally allowed unescaped in urls.
 -}
fromUrl :: String -> IO Repo
fromUrl url
	| not (isURI url) = fromUrlStrict $ escapeURIString isUnescapedInURI url
	| otherwise = fromUrlStrict url

fromUrlStrict :: String -> IO Repo
fromUrlStrict url
	| "file://" `isPrefixOf` url = fromAbsPath $ toRawFilePath $
		unEscapeString $ uriPath u
	| otherwise = pure $ newFrom $ Url u
  where
	u = fromMaybe bad $ parseURI url
	bad = error $ "bad url " ++ url

{- Creates a repo that has an unknown location. -}
fromUnknown :: Repo
fromUnknown = newFrom Unknown

{- Converts a local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = case Url.authority reference of
		Nothing -> r
		Just auth -> 
			let absurl = concat
				[ Url.scheme reference
				, "//"
				, auth
				, fromRawFilePath (repoPath r)
				]
			in r { location = Url $ fromJust $ parseURI absurl }

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
fromRemotes :: Repo -> IO [Repo]
fromRemotes repo = mapM construct remotepairs
  where
	filterconfig f = filter f $ M.toList $ config repo
	filterkeys f = filterconfig (\(k,_) -> f k)
	remotepairs = filterkeys isRemoteKey
	construct (k,v) = remoteNamedFromKey k $
		fromRemoteLocation (fromConfigValue v) repo

{- Sets the name of a remote when constructing the Repo to represent it. -}
remoteNamed :: String -> IO Repo -> IO Repo
remoteNamed n constructor = do
	r <- constructor
	return $ r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
 - "remote.foo.url". -}
remoteNamedFromKey :: ConfigKey -> IO Repo -> IO Repo
remoteNamedFromKey = remoteNamed . remoteKeyToRemoteName

{- Constructs a new Repo for one of a Repo's remotes using a given
 - location (ie, an url). -}
fromRemoteLocation :: String -> Repo -> IO Repo
fromRemoteLocation s repo = gen $ parseRemoteLocation s repo
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
expandTilde = expandt True
  where
	expandt _ [] = return ""
	expandt _ ('/':cs) = do
		v <- expandt True cs
		return ('/':v)
	expandt True ('~':'/':cs) = do
		h <- myHomeDir
		return $ h </> cs
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
			check isBareRepo $
				return Nothing
  where
	check test cont = maybe cont (return . Just) =<< test
	checkdir c = ifM c
		( return $ Just $ LocalUnknown $ toRawFilePath dir
		, return Nothing
		)
	isRepo = checkdir $ 
		gitSignature (".git" </> "config")
			<||>
		-- A git-worktree lacks .git/config, but has .git/commondir.
		-- (Normally the .git is a file, not a symlink, but it can
		-- be converted to a symlink and git will still work;
		-- this handles that case.)
		gitSignature (".git" </> "gitdir")
	isBareRepo = checkdir $ gitSignature "config"
		<&&> doesDirectoryExist (dir </> "objects")
	gitSignature file = doesFileExist $ dir </> file

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
	}
