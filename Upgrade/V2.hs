{- git-annex v2 -> v3 upgrade support
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Upgrade.V2 where

import Annex.Common
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Annex.Branch
import qualified Annex
import Annex.Content
import Utility.Tmp
import Logs
import Messages.Progress

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
	bare <- fromRepo Git.repoIsLocalBare
	old <- fromRepo olddir

	Annex.Branch.create
	showProgressDots

	e <- liftIO $ doesDirectoryExist old
	when e $ do
		config <- Annex.getGitConfig
		mapM_ (\(k, f) -> inject f $ locationLogFile config k) =<< locationLogs
		mapM_ (\f -> inject f f) =<< logFiles old

	saveState False
	showProgressDots

	when e $ do
		inRepo $ Git.Command.run [Param "rm", Param "-r", Param "-f", Param "-q", File old]
		unless bare $ inRepo gitAttributesUnWrite
	showProgressDots

	unless bare push

	return True

locationLogs :: Annex [(Key, FilePath)]
locationLogs = do
	dir <- fromRepo gitStateDir
	liftIO $ do
		levela <- dirContents dir
		levelb <- mapM tryDirContents levela
		files <- mapM tryDirContents (concat levelb)
		return $ mapMaybe islogfile (concat files)
  where
	tryDirContents d = catchDefaultIO [] $ dirContents d
	islogfile f = maybe Nothing (\k -> Just (k, f)) $
			locationLogFileKey f

inject :: FilePath -> FilePath -> Annex ()
inject source dest = do
	old <- fromRepo olddir
	new <- liftIO (readFile $ old </> source)
	Annex.Branch.change dest $ \prev -> 
		unlines $ nub $ lines prev ++ lines new

logFiles :: FilePath -> Annex [FilePath]
logFiles dir = return . filter (".log" `isSuffixOf`)
		<=< liftIO $ getDirectoryContents dir

push :: Annex ()
push = do
	origin_master <- inRepo $ Git.Ref.exists $ Git.Ref "origin/master"
	origin_gitannex <- Annex.Branch.hasOrigin
	case (origin_master, origin_gitannex) of
		(_, True) -> do
			-- Merge in the origin's git-annex branch,
			-- so that pushing the git-annex branch
			-- will immediately work. Not pushed here,
			-- because it's less obnoxious to let the user
			-- push.
			Annex.Branch.update
		(True, False) -> do
			-- push git-annex to origin, so that
			-- "git push" will from then on
			-- automatically push it
			Annex.Branch.update -- just in case
			showAction "pushing new git-annex branch to origin"
			showOutput
			inRepo $ Git.Command.run
				[ Param "push"
				, Param "origin"
				, Param $ Git.fromRef Annex.Branch.name
				]
		_ -> do
			-- no origin exists, so just let the user
			-- know about the new branch
			Annex.Branch.update
			showLongNote $
				"git-annex branch created\n" ++
				"Be sure to push this branch when pushing to remotes.\n"

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
		Git.Command.run [Param "add", File attributes] repo

stateDir :: FilePath
stateDir = addTrailingPathSeparator ".git-annex"
gitStateDir :: Git.Repo -> FilePath
gitStateDir repo = addTrailingPathSeparator $ Git.repoPath repo </> stateDir
