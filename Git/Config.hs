{- git repository configuration handling
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Config where

import qualified Data.Map as M
import Data.Char
import System.Process (cwd, env)

import Common
import Git
import Git.Types
import qualified Git.Construct
import Utility.UserInfo

{- Returns a single git config setting, or a default value if not set. -}
get :: String -> String -> Repo -> String
get key defaultValue repo = M.findWithDefault defaultValue key (config repo)

{- Returns a list with each line of a multiline config setting. -}
getList :: String -> Repo -> [String]
getList key repo = M.findWithDefault [] key (fullconfig repo)

{- Returns a single git config setting, if set. -}
getMaybe :: String -> Repo -> Maybe String
getMaybe key repo = M.lookup key (config repo)

{- Runs git config and populates a repo with its config.
 - Avoids re-reading config when run repeatedly. -}
read :: Repo -> IO Repo
read repo@(Repo { config = c })
	| c == M.empty = read' repo
	| otherwise = return repo

{- Reads config even if it was read before. -}
reRead :: Repo -> IO Repo
reRead r = read' $ r
	{ config = M.empty
	, fullconfig = M.empty
	}

{- Cannot use pipeRead because it relies on the config having been already
 - read. Instead, chdir to the repo and run git config.
 -}
read' :: Repo -> IO Repo
read' repo = go repo
  where
	go Repo { location = Local { gitdir = d } } = git_config d
	go Repo { location = LocalUnknown d } = git_config d
	go _ = assertLocal repo $ error "internal"
	git_config d = withHandle StdoutHandle createProcessSuccess p $
		hRead repo
	  where
		params = ["config", "--null", "--list"]
		p = (proc "git" params)
			{ cwd = Just d
			, env = gitEnv repo
			}

{- Gets the global git config, returning a dummy Repo containing it. -}
global :: IO (Maybe Repo)
global = do
	home <- myHomeDir
	ifM (doesFileExist $ home </> ".gitconfig")
		( do
			repo <- Git.Construct.fromUnknown
			repo' <- withHandle StdoutHandle createProcessSuccess p $
				hRead repo
			return $ Just repo'
		, return Nothing
		)
  where
	params = ["config", "--null", "--list", "--global"]
	p = (proc "git" params)

{- Reads git config from a handle and populates a repo with it. -}
hRead :: Repo -> Handle -> IO Repo
hRead repo h = do
	-- We use the FileSystemEncoding when reading from git-config,
	-- because it can contain arbitrary filepaths (and other strings)
	-- in any encoding.
	fileEncoding h
	val <- hGetContentsStrict h
	store val repo

{- Stores a git config into a Repo, returning the new version of the Repo.
 - The git config may be multiple lines, or a single line.
 - Config settings can be updated incrementally.
 -}
store :: String -> Repo -> IO Repo
store s repo = do
	let c = parse s
	repo' <- updateLocation $ repo
		{ config = (M.map Prelude.head c) `M.union` config repo
		, fullconfig = M.unionWith (++) c (fullconfig repo)
		}
	rs <- Git.Construct.fromRemotes repo'
	return $ repo' { remotes = rs }

{- Updates the location of a repo, based on its configuration.
 -
 - Git.Construct makes LocalUknown repos, of which only a directory is
 - known. Once the config is read, this can be fixed up to a Local repo, 
 - based on the core.bare and core.worktree settings.
 -}
updateLocation :: Repo -> IO Repo
updateLocation r@(Repo { location = LocalUnknown d })
	| isBare r = updateLocation' r $ Local d Nothing
	| otherwise = updateLocation' r $ Local (d </> ".git") (Just d)
updateLocation r@(Repo { location = l@(Local {}) }) = updateLocation' r l
updateLocation r = return r

updateLocation' :: Repo -> RepoLocation -> IO Repo
updateLocation' r l = do
	l' <- case getMaybe "core.worktree" r of
		Nothing -> return l
		Just d -> do
			{- core.worktree is relative to the gitdir -}
			top <- absPath $ gitdir l
			return $ l { worktree = Just $ absPathFrom top d }
	return $ r { location = l' }

{- Parses git config --list or git config --null --list output into a
 - config map. -}
parse :: String -> M.Map String [String]
parse [] = M.empty
parse s
	-- --list output will have an = in the first line
	| all ('=' `elem`) (take 1 ls) = sep '=' ls
	-- --null --list output separates keys from values with newlines
	| otherwise = sep '\n' $ split "\0" s
  where
	ls = lines s
	sep c = M.fromListWith (++) . map (\(k,v) -> (k, [v])) .
		map (separate (== c))

{- Checks if a string from git config is a true value. -}
isTrue :: String -> Maybe Bool
isTrue s
	| s' == "true" = Just True
	| s' == "false" = Just False
	| otherwise = Nothing
  where
	s' = map toLower s

boolConfig :: Bool -> String
boolConfig True = "true"
boolConfig False = "false"

isBare :: Repo -> Bool
isBare r = fromMaybe False $ isTrue =<< getMaybe "core.bare" r
