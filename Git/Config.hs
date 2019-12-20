{- git repository configuration handling
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Git.Config where

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Char
import qualified System.FilePath.ByteString as P

import Common
import Git
import Git.Types
import qualified Git.Command
import qualified Git.Construct
import Utility.UserInfo

{- Returns a single git config setting, or a fallback value if not set. -}
get :: ConfigKey -> ConfigValue -> Repo -> ConfigValue
get key fallback repo = M.findWithDefault fallback key (config repo)

{- Returns a list of values. -}
getList :: ConfigKey -> Repo -> [ConfigValue]
getList key repo = M.findWithDefault [] key (fullconfig repo)

{- Returns a single git config setting, if set. -}
getMaybe :: ConfigKey -> Repo -> Maybe ConfigValue
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
			{ cwd = Just (fromRawFilePath d)
			, env = gitEnv repo
			}

{- Gets the global git config, returning a dummy Repo containing it. -}
global :: IO (Maybe Repo)
global = do
	home <- myHomeDir
	ifM (doesFileExist $ home </> ".gitconfig")
		( do
			repo <- withHandle StdoutHandle createProcessSuccess p $
				hRead (Git.Construct.fromUnknown)
			return $ Just repo
		, return Nothing
		)
  where
	params = ["config", "--null", "--list", "--global"]
	p = (proc "git" params)

{- Reads git config from a handle and populates a repo with it. -}
hRead :: Repo -> Handle -> IO Repo
hRead repo h = do
	val <- S.hGetContents h
	store val repo

{- Stores a git config into a Repo, returning the new version of the Repo.
 - The git config may be multiple lines, or a single line.
 - Config settings can be updated incrementally.
 -}
store :: S.ByteString -> Repo -> IO Repo
store s repo = do
	let c = parse s
	updateLocation $ repo
		{ config = (M.map Prelude.head c) `M.union` config repo
		, fullconfig = M.unionWith (++) c (fullconfig repo)
		}

{- Stores a single config setting in a Repo, returning the new version of
 - the Repo. Config settings can be updated incrementally. -}
store' :: ConfigKey -> ConfigValue -> Repo -> Repo
store' k v repo = repo
	{ config = M.singleton k v `M.union` config repo
	, fullconfig = M.unionWith (++) (M.singleton k [v]) (fullconfig repo)
	}

{- Updates the location of a repo, based on its configuration.
 -
 - Git.Construct makes LocalUknown repos, of which only a directory is
 - known. Once the config is read, this can be fixed up to a Local repo, 
 - based on the core.bare and core.worktree settings.
 -}
updateLocation :: Repo -> IO Repo
updateLocation r@(Repo { location = LocalUnknown d })
	| isBare r = ifM (doesDirectoryExist (fromRawFilePath dotgit))
			( updateLocation' r $ Local dotgit Nothing
			, updateLocation' r $ Local d Nothing
			)
	| otherwise = updateLocation' r $ Local dotgit (Just d)
  where
	dotgit = d P.</> ".git"
updateLocation r@(Repo { location = l@(Local {}) }) = updateLocation' r l
updateLocation r = return r

updateLocation' :: Repo -> RepoLocation -> IO Repo
updateLocation' r l = do
	l' <- case getMaybe "core.worktree" r of
		Nothing -> return l
		Just (ConfigValue d) -> do
			{- core.worktree is relative to the gitdir -}
			top <- absPath $ fromRawFilePath (gitdir l)
			let p = absPathFrom top (fromRawFilePath d)
			return $ l { worktree = Just (toRawFilePath p) }
	return $ r { location = l' }

{- Parses git config --list or git config --null --list output into a
 - config map. -}
parse :: S.ByteString -> M.Map ConfigKey [ConfigValue]
parse s
	| S.null s = M.empty
	-- --list output will have a '=' in the first line
	-- (The first line of --null --list output is the name of a key,
	-- which is assumed to never contain '='.)
	| S.elem eq firstline = sep eq $ S.split nl s
	-- --null --list output separates keys from values with newlines
	| otherwise = sep nl $ S.split 0 s
  where
	nl = fromIntegral (ord '\n')
	eq = fromIntegral (ord '=')
	firstline = S.takeWhile (/= nl) s

	sep c = M.fromListWith (++)
		. map (\(k,v) -> (ConfigKey k, [ConfigValue (S.drop 1 v)])) 
		. map (S.break (== c))

{- Checks if a string from git config is a true/false value. -}
isTrueFalse :: String -> Maybe Bool
isTrueFalse = isTrueFalse' . ConfigValue . encodeBS'

isTrueFalse' :: ConfigValue -> Maybe Bool
isTrueFalse' (ConfigValue s)
	| s' == "true" = Just True
	| s' == "false" = Just False
	| otherwise = Nothing
  where
	s' = S8.map toLower s

boolConfig :: Bool -> String
boolConfig True = "true"
boolConfig False = "false"

boolConfig' :: Bool -> S.ByteString
boolConfig' True = "true"
boolConfig' False = "false"

isBare :: Repo -> Bool
isBare r = fromMaybe False $ isTrueFalse' =<< getMaybe coreBare r

coreBare :: ConfigKey
coreBare = "core.bare"

{- Runs a command to get the configuration of a repo,
 - and returns a repo populated with the configuration, as well as the raw
 - output of the command. -}
fromPipe :: Repo -> String -> [CommandParam] -> IO (Either SomeException (Repo, S.ByteString))
fromPipe r cmd params = try $
	withHandle StdoutHandle createProcessSuccess p $ \h -> do
		val <- S.hGetContents h
		r' <- store val r
		return (r', val)
  where
	p = proc cmd $ toCommand params

{- Reads git config from a specified file and returns the repo populated
 - with the configuration. -}
fromFile :: Repo -> FilePath -> IO (Either SomeException (Repo, S.ByteString))
fromFile r f = fromPipe r "git"
	[ Param "config"
	, Param "--file"
	, File f
	, Param "--list"
	]

{- Changes a git config setting in the specified config file.
 - (Creates the file if it does not already exist.) -}
changeFile :: FilePath -> ConfigKey -> S.ByteString -> IO Bool
changeFile f (ConfigKey k) v = boolSystem "git"
	[ Param "config"
	, Param "--file"
	, File f
	, Param (decodeBS' k)
	, Param (decodeBS' v)
	]

{- Unsets a git config setting, in both the git repo,
 - and the cached config in the Repo.
 -
 - If unsetting the config fails, including in a read-only repo, or
 - when the config is not set, returns Nothing.
 -}
unset :: ConfigKey -> Repo -> IO (Maybe Repo)
unset ck@(ConfigKey k) r = ifM (Git.Command.runBool ps r)
	( return $ Just $ r { config = M.delete ck (config r) }
	, return Nothing
	)
  where
	ps = [Param "config", Param "--unset-all", Param (decodeBS' k)]
