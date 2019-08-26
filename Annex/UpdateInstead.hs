{- git-annex UpdateIntead emulation
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.UpdateInstead where

import qualified Annex
import Annex.Common
import Annex.Version
import Annex.AdjustedBranch
import Git.Branch
import Git.ConfigTypes

{- receive.denyCurrentBranch=updateInstead does not work
 - when an adjusted branch is checked out, so must be emulated. -}
needUpdateInsteadEmulation :: Annex Bool
needUpdateInsteadEmulation = updateinsteadset <&&> isadjusted
  where
	updateinsteadset = (== UpdateInstead) . receiveDenyCurrentBranch
		<$> Annex.getGitConfig
	isadjusted = versionSupportsUnlockedPointers
		<&&> (maybe False (isJust . getAdjustment) <$> inRepo Git.Branch.current)
