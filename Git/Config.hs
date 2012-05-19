{- git repository configuration handling
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Config where

import qualified Data.Map as M
import Data.Char

import Common
import Git
import Git.Types
import qualified Git.Construct

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
reRead = read'

{- Cannot use pipeRead because it relies on the config having been already
 - read. Instead, chdir to the repo.
 -}
read' :: Repo -> IO Repo
read' repo = go repo
	where
		go Repo { location = Local { gitdir = d } } = git_config d
		go Repo { location = LocalUnknown d } = git_config d
		go _ = assertLocal repo $ error "internal"
		git_config d = bracketCd d $
			pOpen ReadFromPipe "git" ["config", "--null", "--list"] $
				hRead repo

{- Reads git config from a handle and populates a repo with it. -}
hRead :: Repo -> Handle -> IO Repo
hRead repo h = do
	val <- hGetContentsStrict h
	store val repo

{- Stores a git config into a Repo, returning the new version of the Repo.
 - The git config may be multiple lines, or a single line.
 - Config settings can be updated incrementally.
 -}
store :: String -> Repo -> IO Repo
store s repo = do
	let c = parse s
	let repo' = updateLocation $ repo
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
updateLocation :: Repo -> Repo
updateLocation r@(Repo { location = LocalUnknown d })
	| isBare r = newloc $ Local d Nothing
	| otherwise = newloc $ Local (d </> ".git") (Just d)
	where
		newloc l = r { location = getworktree l }
		getworktree l = case workTree r of
			Nothing -> l
			wt -> l { worktree = wt }
updateLocation r = r

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

isBare :: Repo -> Bool
isBare r = fromMaybe False $ isTrue =<< getMaybe "core.bare" r

workTree :: Repo -> Maybe FilePath
workTree = getMaybe "core.worktree"
