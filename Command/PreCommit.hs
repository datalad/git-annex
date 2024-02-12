{- git-annex command
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.PreCommit where

import Command
import qualified Command.Fix
import qualified Command.Smudge
import Annex.Hook
import Annex.Link
import Annex.View
import Annex.View.ViewedFile
import Logs.View
import Logs.MetaData
import Types.View
import Types.MetaData
import qualified Annex
import qualified Annex.Branch

import qualified Data.Set as S
import qualified Data.Text as T

cmd :: Command
cmd = noCommit $ command "pre-commit" SectionPlumbing
	"run by git pre-commit hook"
	paramPaths
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek ps = do
	let ww = WarnUnmatchWorkTreeItems "pre-commit"
	l <- workTreeItems ww ps
	-- fix symlinks to files being committed
	flip (withFilesToBeCommitted ww) l $ \(si, f) -> commandAction $
		maybe stop (Command.Fix.start Command.Fix.FixSymlinks si f)
			=<< isAnnexLink f
	-- after a merge conflict or git cherry-pick or stash, pointer
	-- files in the worktree won't be populated, so populate them here
	Command.Smudge.updateSmudged (Restage False)
	
	runAnnexHook preCommitAnnexHook

	-- committing changes to a view updates metadata
	currentView >>= \case
		Nothing -> noop
		Just (v, _madj) -> do
			withViewChanges
				(addViewMetaData v)
				(removeViewMetaData v)
			-- Manually commit in this case, because
			-- noCommit prevents automatic commit.
			whenM (annexAlwaysCommit <$> Annex.getGitConfig) $
				Annex.Branch.commit =<< Annex.Branch.commitMessage

addViewMetaData :: View -> ViewedFile -> Key -> CommandStart
addViewMetaData v f k = starting "metadata" ai si $
	next $ changeMetaData k $ fromView v f
  where
	ai = mkActionItem (k, toRawFilePath f)
	si = SeekInput []

removeViewMetaData :: View -> ViewedFile -> Key -> CommandStart
removeViewMetaData v f k = starting "metadata" ai si $
	next $ changeMetaData k $ unsetMetaData $ fromView v f
  where
	ai = mkActionItem (k, toRawFilePath f)
	si = SeekInput []

changeMetaData :: Key -> MetaData -> CommandCleanup
changeMetaData k metadata = do
	showMetaDataChange metadata
	addMetaData k metadata
	return True

showMetaDataChange :: MetaData -> Annex ()
showMetaDataChange = showLongNote . UnquotedString . unlines . concatMap showmeta . fromMetaData
  where
	showmeta (f, vs) = map (showmetavalue f) $ S.toList vs
	showmetavalue f v = T.unpack (fromMetaField f) <> showset v <> "=" <> decodeBS (fromMetaValue v)
	showset v
		| isSet v = "+"
		| otherwise = "-"
