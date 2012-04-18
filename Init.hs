{- git-annex repository initialization
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Init (
	ensureInitialized,
	initialize,
	uninitialize
) where

import Common.Annex
import Utility.TempFile
import qualified Git
import qualified Annex.Branch
import Logs.UUID
import Annex.Version
import Annex.UUID

initialize :: Maybe String -> Annex ()
initialize mdescription = do
	prepUUID
	Annex.Branch.create
	setVersion
	gitPreCommitHookWrite
	u <- getUUID
	maybe (recordUUID u) (describeUUID u) mdescription

uninitialize :: Annex ()
uninitialize = gitPreCommitHookUnWrite

{- Will automatically initialize if there is already a git-annex
   branch from somewhere. Otherwise, require a manual init
   to avoid git-annex accidentially being run in git
   repos that did not intend to use it. -}
ensureInitialized :: Annex ()
ensureInitialized = getVersion >>= maybe needsinit checkVersion
	where
		needsinit = ifM Annex.Branch.hasSibling
				( initialize Nothing
				, error "First run: git-annex init"
				)

{- set up a git pre-commit hook, if one is not already present -}
gitPreCommitHookWrite :: Annex ()
gitPreCommitHookWrite = unlessBare $ do
	hook <- preCommitHook
	ifM (liftIO $ doesFileExist hook)
		( warning $ "pre-commit hook (" ++ hook ++ ") already exists, not configuring"
		, liftIO $ do
			viaTmp writeFile hook preCommitScript
			p <- getPermissions hook
			setPermissions hook $ p {executable = True}
		)

gitPreCommitHookUnWrite :: Annex ()
gitPreCommitHookUnWrite = unlessBare $ do
	hook <- preCommitHook
	whenM (liftIO $ doesFileExist hook) $
		ifM (liftIO $ (==) preCommitScript <$> readFile hook)
			( liftIO $ removeFile hook
			, warning $ "pre-commit hook (" ++ hook ++ 
				") contents modified; not deleting." ++
				" Edit it to remove call to git annex."
			)

unlessBare :: Annex () -> Annex ()
unlessBare = unlessM $ fromRepo Git.repoIsLocalBare

preCommitHook :: Annex FilePath
preCommitHook = (</>) <$> fromRepo Git.gitDir <*> pure "hooks/pre-commit"

preCommitScript :: String
preCommitScript = 
	"#!/bin/sh\n" ++
	"# automatically configured by git-annex\n" ++ 
	"git annex pre-commit .\n"
