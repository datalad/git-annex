{- git-annex direct mode
 -
 - This only contains some remnants needed to convert away from direct mode.
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Direct (
	switchHEADBack,
	setIndirect,
) where

import Annex.Common
import qualified Annex
import qualified Git.Config
import qualified Git.Ref
import qualified Git.Branch
import Git.Types
import Config

setIndirect :: Annex ()
setIndirect = do
	setbare
	switchHEADBack
	setConfig (annexConfig "direct") val
	Annex.changeGitConfig $ \c -> c { annexDirect = False }
  where
	val = Git.Config.boolConfig False
	coreworktree = ConfigKey "core.worktree"
	indirectworktree = ConfigKey "core.indirect-worktree"
	setbare = do
		-- core.worktree is not compatable with
		-- core.bare; git does not allow both to be set, so
		-- unset it when enabling direct mode, caching in
		-- core.indirect-worktree
		moveconfig indirectworktree coreworktree
		setConfig (ConfigKey Git.Config.coreBare) val
	moveconfig src dest = getConfigMaybe src >>= \case
		Nothing -> noop
		Just wt -> do
			unsetConfig src
			setConfig dest wt
			reloadConfig

{- Converts a directBranch back to the original branch.
 -
 - Any other ref is left unchanged.
 -}
fromDirectBranch :: Ref -> Ref
fromDirectBranch directhead = case splitc '/' $ fromRef directhead of
	("refs":"heads":"annex":"direct":rest) -> 
		Ref $ "refs/heads/" ++ intercalate "/" rest
	_ -> directhead

switchHEADBack :: Annex ()
switchHEADBack = maybe noop switch =<< inRepo Git.Branch.currentUnsafe
  where
	switch currhead = do
		let orighead = fromDirectBranch currhead
		inRepo (Git.Ref.sha currhead) >>= \case
			Just headsha
				| orighead /= currhead -> do
					inRepo $ Git.Branch.update "leaving direct mode" orighead headsha
					inRepo $ Git.Branch.checkout orighead
					inRepo $ Git.Branch.delete currhead
			_ -> inRepo $ Git.Branch.checkout orighead
