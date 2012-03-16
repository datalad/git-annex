{- git repository configuration handling
 -
 - Copyright 2010,2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Config where

import System.Posix.Directory
import Control.Exception (bracket_)
import qualified Data.Map as M

import Common
import Git
import Git.Types
import qualified Git.Construct

{- Returns a single git config setting, or a default value if not set. -}
get :: String -> String -> Repo -> String
get key defaultValue repo = M.findWithDefault defaultValue key (config repo)

{- Returns a single git config setting, if set. -}
getMaybe :: String -> Repo -> Maybe String
getMaybe key repo = M.lookup key (config repo)

{- Runs git config and populates a repo with its config. -}
read :: Repo -> IO Repo
read repo@(Repo { location = Dir d }) = bracketcd d $
	{- Cannot use pipeRead because it relies on the config having
	   been already read. Instead, chdir to the repo. -}
	pOpen ReadFromPipe "git" ["config", "--null", "--list"] $ hRead repo
	where
		bracketcd to a = bracketcd' to a =<< getCurrentDirectory
		bracketcd' to a cwd 
			| dirContains to cwd = a
			| otherwise = bracket_ (changeWorkingDirectory to) (changeWorkingDirectory cwd) a
read r = assertLocal r $
	error $ "internal error; trying to read config of " ++ show r

{- Reads git config from a handle and populates a repo with it. -}
hRead :: Repo -> Handle -> IO Repo
hRead repo h = do
	val <- hGetContentsStrict h
	store val repo

{- Stores a git config into a repo, returning the new version of the repo.
 - The git config may be multiple lines, or a single line. Config settings
 - can be updated inrementally. -}
store :: String -> Repo -> IO Repo
store s repo = do
	let c = parse s
	let repo' = repo
		{ config = (M.map Prelude.head c) `M.union` config repo
		, fullconfig = M.unionWith (++) c (fullconfig repo)
		}
	rs <- Git.Construct.fromRemotes repo'
	return $ repo' { remotes = rs }

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
