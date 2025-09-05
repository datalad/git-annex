{- git-annex v2 -> v3 upgrade support
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Upgrade.V2 where

import Annex.Common
import Types.Upgrade
import qualified Git
import qualified Git.Command
import qualified Git.Ref
import qualified Annex.Branch
import qualified Annex
import Annex.Content
import Utility.Tmp
import Logs
import Messages.Progress
import qualified Utility.FileIO as F
import qualified Utility.OsString as OS

olddir :: Git.Repo -> OsPath
olddir g
	| Git.repoIsLocalBare g = literalOsPath ""
	| otherwise = literalOsPath ".git-annex"

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
upgrade :: Annex UpgradeResult
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
		inRepo $ Git.Command.run [Param "rm", Param "-r", Param "-f", Param "-q", File (fromOsPath old)]
		unless bare $ inRepo gitAttributesUnWrite
	showProgressDots

	unless bare push

	return UpgradeSuccess

locationLogs :: Annex [(Key, OsPath)]
locationLogs = do
	config <- Annex.getGitConfig
	dir <- fromRepo gitStateDir
	liftIO $ do
		levela <- dirContents dir
		levelb <- mapM tryDirContents levela
		files <- mapM tryDirContents (concat levelb)
		return $ mapMaybe (islogfile config) (concat files)
  where
	tryDirContents d = catchDefaultIO [] $ dirContents d
	islogfile config f = maybe Nothing (\k -> Just (k, f)) $
			locationLogFileKey config f

inject :: OsPath -> OsPath -> Annex ()
inject source dest = do
	old <- fromRepo olddir
	new <- liftIO $ readFileString (old </> source)
	Annex.Branch.change (Annex.Branch.RegardingUUID []) dest $ \prev -> 
		encodeBL $ unlines $ nub $ lines (decodeBL prev) ++ lines new

logFiles :: OsPath -> Annex [OsPath]
logFiles dir = return . filter (literalOsPath ".log" `OS.isSuffixOf`)
		<=< liftIO $ getDirectoryContents dir

push :: Annex ()
push = do
	origin_master <- inRepo $ Git.Ref.exists $ 
		Git.Ref $ encodeBS "origin/master"
	origin_gitannex <- Annex.Branch.hasOrigin
	case (origin_master, origin_gitannex) of
		(_, True) -> do
			-- Merge in the origin's git-annex branch,
			-- so that pushing the git-annex branch
			-- will immediately work. Not pushed here,
			-- because it's less obnoxious to let the user
			-- push.
			void Annex.Branch.update
		(True, False) -> do
			-- push git-annex to origin, so that
			-- "git push" will from then on
			-- automatically push it
			void Annex.Branch.update -- just in case
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
			void Annex.Branch.update
			showLongNote $ UnquotedString $
				"git-annex branch created\n" ++
				"Be sure to push this branch when pushing to remotes.\n"

{- Old .gitattributes contents, not needed anymore. -}
attrLines :: [String]
attrLines =
	[ fromOsPath $ stateDir </> literalOsPath "*.log merge=union"
	, fromOsPath $ stateDir </> literalOsPath "*/*/*.log merge=union"
	]

gitAttributesUnWrite :: Git.Repo -> IO ()
gitAttributesUnWrite repo = do
	let attributes = Git.attributes repo
	whenM (doesFileExist attributes) $ do
		c <- map decodeBS . fileLines'
			<$> F.readFile' attributes
		liftIO $ viaTmp writeFileString attributes 
			(unlines $ filter (`notElem` attrLines) c)
		Git.Command.run [Param "add", File (fromOsPath attributes)] repo

stateDir :: OsPath
stateDir = addTrailingPathSeparator (literalOsPath ".git-annex")

gitStateDir :: Git.Repo -> OsPath
gitStateDir repo = addTrailingPathSeparator $ Git.repoPath repo </> stateDir
