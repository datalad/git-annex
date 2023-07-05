{- git repository configuration handling
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
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
import Control.Concurrent.Async

import Common
import Git
import Git.Types
import qualified Git.Command
import qualified Git.Construct
import Utility.UserInfo
import Utility.Process.Transcript
import Utility.Debug

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
	-- Passing --git-dir changes git's behavior when run in a
	-- repository belonging to another user. When the git directory
	-- was explicitly specified, pass that in order to get the local
	-- git config.
	go Repo { location = Local { gitdir = d } }
		| gitDirSpecifiedExplicitly repo = git_config ["--git-dir=."] d
	-- Run in worktree when there is one, since running in the .git
	-- directory will trigger safe.bareRepository=explicit, even
	-- when not in a bare repository.
	go Repo { location = Local { worktree = Just d } } = git_config [] d
	go Repo { location = Local { gitdir = d } } = git_config [] d
	go Repo { location = LocalUnknown d } = git_config [] d
	go _ = assertLocal repo $ error "internal"
	git_config addparams d = withCreateProcess p (git_config' p)
	  where
		params = addparams ++ ["config", "--null", "--list"]
		p = (proc "git" params)
			{ cwd = Just (fromRawFilePath d)
			, env = gitEnv repo
			, std_out = CreatePipe 
			}
	git_config' p _ (Just hout) _ pid = 
		forceSuccessProcess p pid
			`after`
		hRead repo ConfigNullList hout
	git_config' _ _ _ _ _ = error "internal"

{- Gets the global git config, returning a dummy Repo containing it. -}
global :: IO (Maybe Repo)
global = do
	home <- myHomeDir
	ifM (doesFileExist $ home </> ".gitconfig")
		( Just <$> withCreateProcess p go
		, return Nothing
		)
  where
	params = ["config", "--null", "--list", "--global"]
	p = (proc "git" params)
		{ std_out = CreatePipe }
	go _ (Just hout) _ pid = 
		forceSuccessProcess p pid
			`after`
		hRead (Git.Construct.fromUnknown) ConfigNullList hout
	go _ _ _ _ = error "internal"

{- Reads git config from a handle and populates a repo with it. -}
hRead :: Repo -> ConfigStyle -> Handle -> IO Repo
hRead repo st h = do
	val <- S.hGetContents h
	let c = parse val st
	debug (DebugSource "Git.Config") $ "git config read: " ++
		show (map (\(k, v) -> (show k, map show v)) (M.toList c))
	storeParsed c repo

{- Stores a git config into a Repo, returning the new version of the Repo.
 - The git config may be multiple lines, or a single line.
 - Config settings can be updated incrementally.
 -}
store :: S.ByteString -> ConfigStyle -> Repo -> IO Repo
store s st = storeParsed (parse s st)

storeParsed :: M.Map ConfigKey [ConfigValue] -> Repo -> IO Repo
storeParsed c repo = updateLocation $ repo
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
updateLocation r@(Repo { location = LocalUnknown d }) = case isBare r of
	Just True -> ifM (doesDirectoryExist (fromRawFilePath dotgit))
		( updateLocation' r $ Local dotgit Nothing
		, updateLocation' r $ Local d Nothing
		)
	Just False -> mknonbare
	{- core.bare not in config, probably because safe.directory
	 - did not allow reading the config -}
	Nothing -> ifM (Git.Construct.isBareRepo (fromRawFilePath d))
		( mkbare
		, mknonbare
		)
  where
	dotgit = d P.</> ".git"
	-- git treats eg ~/foo as a bare git repository located in
	-- ~/foo/.git if ~/foo/.git/config has core.bare=true
	mkbare = ifM (doesDirectoryExist (fromRawFilePath dotgit))
		( updateLocation' r $ Local dotgit Nothing
		, updateLocation' r $ Local d Nothing
		)
	mknonbare = updateLocation' r $ Local dotgit (Just d)

updateLocation r@(Repo { location = l@(Local {}) }) = updateLocation' r l
updateLocation r = return r

updateLocation' :: Repo -> RepoLocation -> IO Repo
updateLocation' r l = do
	l' <- case getMaybe "core.worktree" r of
		Nothing -> return l
		Just (ConfigValue d) -> do
			{- core.worktree is relative to the gitdir -}
			top <- absPath (gitdir l)
			let p = absPathFrom top d
			return $ l { worktree = Just p }
		Just NoConfigValue -> return l
	return $ r { location = l' }

data ConfigStyle = ConfigList | ConfigNullList

{- Parses git config --list or git config --null --list output into a
 - config map. -}
parse :: S.ByteString -> ConfigStyle -> M.Map ConfigKey [ConfigValue]
parse s st
	| S.null s = M.empty
	| otherwise = case st of
		ConfigList -> sep eq $ S.split nl s
		ConfigNullList -> sep nl $ S.split 0 s
  where
	nl = fromIntegral (ord '\n')
	eq = fromIntegral (ord '=')

	sep c = M.fromListWith (++)
		. map (\(k,v) -> (ConfigKey k, [mkval v])) 
		. map (S.break (== c))
	
	mkval v 
		| S.null v = NoConfigValue
		| otherwise = ConfigValue (S.drop 1 v)

{- Checks if a string from git config is a true/false value. -}
isTrueFalse :: String -> Maybe Bool
isTrueFalse = isTrueFalse' . ConfigValue . encodeBS

isTrueFalse' :: ConfigValue -> Maybe Bool
isTrueFalse' (ConfigValue s)
	| s' == "yes" = Just True
	| s' == "on" = Just True
	| s' == "true" = Just True
	| s' == "1" = Just True

	| s' == "no" = Just False
	| s' == "off" = Just False
	| s' == "false" = Just False
	| s' == "0" = Just False
	| s' == "" = Just False

	-- Git treats any number other than 0 as true,
	-- including negative numbers.
	| S8.all (\c -> isDigit c || c == '-') s' = Just True

	| otherwise = Nothing
  where
	s' = S8.map toLower s
isTrueFalse' NoConfigValue = Just True

boolConfig :: Bool -> String
boolConfig True = "true"
boolConfig False = "false"

boolConfig' :: Bool -> S.ByteString
boolConfig' True = "true"
boolConfig' False = "false"

{- Note that repoIsLocalBare is often better to use than this. -}
isBare :: Repo -> Maybe Bool
isBare r = isTrueFalse' =<< getMaybe coreBare r

coreBare :: ConfigKey
coreBare = "core.bare"

{- Runs a command to get the configuration of a repo,
 - and returns a repo populated with the configuration, as well as the raw
 - output and the standard error of the command. -}
fromPipe :: Repo -> String -> [CommandParam] -> ConfigStyle -> IO (Either SomeException (Repo, S.ByteString, String))
fromPipe r cmd params st = tryNonAsync $ withCreateProcess p go
  where
	p = (proc cmd $ toCommand params)
		{ std_out = CreatePipe
		, std_err = CreatePipe
		}
	go _ (Just hout) (Just herr) pid =
		withAsync (getstderr pid herr []) $ \errreader -> do
			val <- S.hGetContents hout
			err <- wait errreader
			forceSuccessProcess p pid
			r' <- store val st r
			return (r', val, err)
	go _ _ _ _ = error "internal"

	getstderr pid herr c = hGetLineUntilExitOrEOF pid herr >>= \case
		Just l -> getstderr pid herr (l:c)
		Nothing -> return (unlines (reverse c))

{- Reads git config from a specified file and returns the repo populated
 - with the configuration. -}
fromFile :: Repo -> FilePath -> IO (Either SomeException (Repo, S.ByteString, String))
fromFile r f = fromPipe r "git"
	[ Param "config"
	, Param "--file"
	, File f
	, Param "--list"
	] ConfigList

{- Changes a git config setting in .git/config. -}
change :: ConfigKey -> S.ByteString -> Repo -> IO Bool
change (ConfigKey k) v = Git.Command.runBool
	[ Param "config"
	, Param (decodeBS k)
	, Param (decodeBS v)
	]

{- Changes a git config setting in the specified config file.
 - (Creates the file if it does not already exist.) -}
changeFile :: FilePath -> ConfigKey -> S.ByteString -> IO Bool
changeFile f (ConfigKey k) v = boolSystem "git"
	[ Param "config"
	, Param "--file"
	, File f
	, Param (decodeBS k)
	, Param (decodeBS v)
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
	ps = [Param "config", Param "--unset-all", Param (decodeBS k)]

{- git "fixed" CVE-2022-24765 by preventing git-config from
 - listing per-repo configs when the repo is not owned by
 - the current user. Detect if this fix is in effect for the
 - repo.
 -}
checkRepoConfigInaccessible :: Repo -> IO Bool
checkRepoConfigInaccessible r
	-- When --git-dir or GIT_DIR is used to specify the git
	-- directory, git does not check for CVE-2022-24765.
	| gitDirSpecifiedExplicitly r = return False
	| otherwise = do
		-- Cannot use gitCommandLine here because specifying --git-dir
		-- will bypass the git security check.
		let p = (proc "git" ["config", "--local", "--list"])
			{ cwd = Just (fromRawFilePath (repoPath r))
			, env = gitEnv r
			}
		(out, ok) <- processTranscript' p Nothing
		if not ok
			then do
				debug (DebugSource "Git.Config") ("config output: " ++ out)
				return True
			else return False
