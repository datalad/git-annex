{- git-annex v2 -> v3 upgrade support
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V2 where

import System.Directory
import System.FilePath
import Control.Monad.State (unless, when, liftIO)
import Data.List
import Data.Maybe

import Types.Key
import Types
import qualified Annex
import qualified Git
import qualified Branch
import Messages
import Utility
import LocationLog
import Content

olddir :: Git.Repo -> FilePath
olddir g
	| Git.repoIsLocalBare g = ""
	| otherwise = ".git-annex"

{- .git-annex/ moved to a git-annex branch.
 - 
 - Strategy:
 - 
 - * Create the git-annex branch.
 - * Find each location log file in .git-annex/, and inject its content
 -   into the git-annex branch, unioning with any content already in
 -   there. (in passing, this deals with the semi transition that left
 -   some location logs hashed two different ways; both are found and
 -   merged).
 - * Also inject remote.log, trust.log, and uuid.log.
 - * git rm -rf .git-annex
 - * Remove stuff that used to be needed in .gitattributes.
 - * Commit changes.
 -}
upgrade :: Annex Bool
upgrade = do
	showAction "v2 to v3"
	g <- Annex.gitRepo
	let bare = Git.repoIsLocalBare g

	Branch.create
	showProgress

	e <- liftIO $ doesDirectoryExist (olddir g)
	when e $ do
		mapM_ (\(k, f) -> inject f $ logFile k) =<< locationLogs g
		mapM_ (\f -> inject f f) =<< logFiles (olddir g)

	saveState
	showProgress

	when e $ liftIO $ do
		Git.run g "rm" [Param "-r", Param "-f", Param "-q", File (olddir g)]
		unless bare $ gitAttributesUnWrite g
	showProgress

	unless bare push

	return True

locationLogs :: Git.Repo -> Annex [(Key, FilePath)]
locationLogs repo = liftIO $ do
	levela <- dirContents dir
	levelb <- mapM tryDirContents levela
	files <- mapM tryDirContents (concat levelb)
	return $ mapMaybe islogfile (concat files)
	where
		tryDirContents d = catch (dirContents d) (return . const [])
		dir = gitStateDir repo
		islogfile f = maybe Nothing (\k -> Just (k, f)) $
				logFileKey $ takeFileName f

inject :: FilePath -> FilePath -> Annex ()
inject source dest = do
	g <- Annex.gitRepo
	new <- liftIO (readFile $ olddir g </> source)
	prev <- Branch.get dest
	Branch.change dest $ unlines $ nub $ lines prev ++ lines new
	showProgress

logFiles :: FilePath -> Annex [FilePath]
logFiles dir = return . filter (".log" `isSuffixOf`)
		=<< liftIO (getDirectoryContents dir)

push :: Annex ()
push = do
	origin_master <- Branch.refExists "origin/master"
	origin_gitannex <- Branch.hasOrigin
	case (origin_master, origin_gitannex) of
		(_, True) -> do
			-- Merge in the origin's git-annex branch,
			-- so that pushing the git-annex branch
			-- will immediately work. Not pushed here,
			-- because it's less obnoxious to let the user
			-- push.
			Branch.update
		(True, False) -> do
			-- push git-annex to origin, so that
			-- "git push" will from then on
			-- automatically push it
			Branch.update -- just in case
			showAction "pushing new git-annex branch to origin"
			showOutput
			g <- Annex.gitRepo
			liftIO $ Git.run g "push" [Param "origin", Param Branch.name]
		_ -> do
			-- no origin exists, so just let the user
			-- know about the new branch
			Branch.update
			showLongNote $
				"git-annex branch created\n" ++
				"Be sure to push this branch when pushing to remotes.\n"
			showOutput

{- Old .gitattributes contents, not needed anymore. -}
attrLines :: [String]
attrLines =
	[ stateDir </> "*.log merge=union"
	, stateDir </> "*/*/*.log merge=union"
	]

gitAttributesUnWrite :: Git.Repo -> IO ()
gitAttributesUnWrite repo = do
	let attributes = Git.attributes repo
	whenM (doesFileExist attributes) $ do
		c <- readFileStrict attributes
		liftIO $ viaTmp writeFile attributes $ unlines $
			filter (`notElem` attrLines) $ lines c
		Git.run repo "add" [File attributes]

stateDir :: FilePath
stateDir = addTrailingPathSeparator ".git-annex"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = addTrailingPathSeparator $ Git.workTree repo </> stateDir
