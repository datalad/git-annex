{- Construction of Git Repo objects
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Construct (
	fromCurrent,
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
) where

import System.Posix.User
import System.Posix.Env (getEnv, unsetEnv)
import System.Posix.Directory (changeWorkingDirectory)
import qualified Data.Map as M hiding (map, split)
import Network.URI

import Common
import Git.Types
import Git
import qualified Git.Url as Url

{- Finds the current git repository.
 -
 - GIT_DIR can override the location of the .git directory.
 -
 - When GIT_WORK_TREE is set, chdir to it, so that anything using
 - this repository runs in the right location. However, this chdir is
 - done after determining GIT_DIR; git does not let GIT_WORK_TREE
 - influence the git directory.
 -
 - Both environment variables are unset, to avoid confusing other git
 - commands that also look at them. This would particularly be a problem
 - when GIT_DIR is relative and we chdir for GIT_WORK_TREE. Instead,
 - the Git module passes --work-tree and --git-dir to git commands it runs.
 -}
fromCurrent :: IO Repo
fromCurrent = do
	r <- maybe fromCwd fromPath =<< getEnv "GIT_DIR"
	maybe (return ()) changeWorkingDirectory =<< getEnv "GIT_WORK_TREE"
	unsetEnv "GIT_DIR"
	unsetEnv "GIT_WORK_TREE"
	return r

{- Finds the git repository used for the Cwd, which may be in a parent
 - directory. -}
fromCwd :: IO Repo
fromCwd = getCurrentDirectory >>= seekUp isRepoTop >>= maybe norepo makerepo
	where
		makerepo = newFrom . Dir
		norepo = error "Not in a git repository."

{- Local Repo constructor, accepts a relative or absolute path. -}
fromPath :: FilePath -> IO Repo
fromPath dir = fromAbsPath =<< absPath dir

{- Local Repo constructor, requires an absolute path to the repo be
 - specified. -}
fromAbsPath :: FilePath -> IO Repo
fromAbsPath dir
	| "/" `isPrefixOf` dir = do
 		-- Git always looks for "dir.git" in preference to
		-- to "dir", even if dir ends in a "/".
		let canondir = dropTrailingPathSeparator dir
		let dir' = canondir ++ ".git"
		e <- doesDirectoryExist dir'
		if e
			then ret dir'
			else if "/.git" `isSuffixOf` canondir
				then do
					-- When dir == "foo/.git", git looks
					-- for "foo/.git/.git", and failing
					-- that, uses "foo" as the repository.
					e' <- doesDirectoryExist $ dir </> ".git"
					if e'
						then ret dir
						else ret $ takeDirectory canondir
				else ret dir
	| otherwise = error $ "internal error, " ++ dir ++ " is not absolute"
	where
		ret = newFrom . Dir

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
	| startswith "file://" url = fromAbsPath $ uriPath u
	| otherwise = newFrom $ Url u
	where
		u = fromMaybe bad $ parseURI url
		bad = error $ "bad url " ++ url

{- Creates a repo that has an unknown location. -}
fromUnknown :: IO Repo
fromUnknown = newFrom Unknown

{- Converts a local Repo into a remote repo, using the reference repo
 - which is assumed to be on the same host. -}
localToUrl :: Repo -> Repo -> Repo
localToUrl reference r
	| not $ repoIsUrl reference = error "internal error; reference repo not url"
	| repoIsUrl r = r
	| otherwise = r { location = Url $ fromJust $ parseURI absurl }
	where
		absurl =
			Url.scheme reference ++ "//" ++
			Url.authority reference ++
			workTree r

{- Calculates a list of a repo's configured remotes, by parsing its config. -}
fromRemotes :: Repo -> IO [Repo]
fromRemotes repo = mapM construct remotepairs
	where
		filterconfig f = filter f $ M.toList $ config repo
		filterkeys f = filterconfig (\(k,_) -> f k)
		remotepairs = filterkeys isremote
		isremote k = startswith "remote." k && endswith ".url" k
		construct (k,v) = remoteNamedFromKey k $ fromRemoteLocation v repo

{- Sets the name of a remote when constructing the Repo to represent it. -}
remoteNamed :: String -> IO Repo -> IO Repo
remoteNamed n constructor = do
	r <- constructor
	return $ r { remoteName = Just n }

{- Sets the name of a remote based on the git config key, such as
   "remote.foo.url". -}
remoteNamedFromKey :: String -> IO Repo -> IO Repo
remoteNamedFromKey k = remoteNamed basename
	where
		basename = join "." $ reverse $ drop 1 $
				reverse $ drop 1 $ split "." k

{- Constructs a new Repo for one of a Repo's remotes using a given
 - location (ie, an url). -}
fromRemoteLocation :: String -> Repo -> IO Repo
fromRemoteLocation s repo = gen $ calcloc s
	where
		gen v	
			| scpstyle v = fromUrl $ scptourl v
			| urlstyle v = fromUrl v
			| otherwise = fromRemotePath v repo
		-- insteadof config can rewrite remote location
		calcloc l
			| null insteadofs = l
			| otherwise = replacement ++ drop (length bestvalue) l
			where
				replacement = drop (length prefix) $
					take (length bestkey - length suffix) bestkey
				(bestkey, bestvalue) = maximumBy longestvalue insteadofs
				longestvalue (_, a) (_, b) = compare b a
				insteadofs = filterconfig $ \(k, v) -> 
					startswith prefix k &&
					endswith suffix k &&
					startswith v l
				filterconfig f = filter f $
					concatMap splitconfigs $
						M.toList $ fullconfig repo
				splitconfigs (k, vs) = map (\v -> (k, v)) vs
				(prefix, suffix) = ("url." , ".insteadof")
		urlstyle v = isURI v || ":" `isInfixOf` v && "//" `isInfixOf` v
		-- git remotes can be written scp style -- [user@]host:dir
		scpstyle v = ":" `isInfixOf` v && not ("//" `isInfixOf` v)
		scptourl v = "ssh://" ++ host ++ slash dir
			where
				(host, dir) = separate (== ':') v
				slash d	| d == "" = "/~/" ++ d
					| "/" `isPrefixOf` d = d
					| "~" `isPrefixOf` d = '/':d
					| otherwise = "/~/" ++ d

{- Constructs a Repo from the path specified in the git remotes of
 - another Repo. -}
fromRemotePath :: FilePath -> Repo -> IO Repo
fromRemotePath dir repo = do
	dir' <- expandTilde dir
	fromAbsPath $ workTree repo </> dir'

{- Git remotes can have a directory that is specified relative
 - to the user's home directory, or that contains tilde expansions.
 - This converts such a directory to an absolute path.
 - Note that it has to run on the system where the remote is.
 -}
repoAbsPath :: FilePath -> IO FilePath
repoAbsPath d = do
	d' <- expandTilde d
	h <- myHomeDir
	return $ h </> d'

expandTilde :: FilePath -> IO FilePath
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

seekUp :: (FilePath -> IO Bool) -> FilePath -> IO (Maybe FilePath)
seekUp want dir = do
	ok <- want dir
	if ok
		then return $ Just dir
		else case parentDir dir of
			"" -> return Nothing
			d -> seekUp want d

isRepoTop :: FilePath -> IO Bool
isRepoTop dir = do
	r <- isRepo
	if r
		then return r
		else isBareRepo
	where
		isRepo = gitSignature (".git" </> "config")
		isBareRepo = do
			e <- doesDirectoryExist (dir </> "objects")
			if not e
				then return e
				else gitSignature "config"
		gitSignature file = doesFileExist (dir </> file)

newFrom :: RepoLocation -> IO Repo
newFrom l = return Repo
	{ location = l
	, config = M.empty
	, fullconfig = M.empty
	, remotes = []
	, remoteName = Nothing
	}
