{- git-annex repo sizes, live updates
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Annex.RepoSize.LiveUpdate where

import Annex.Common
import qualified Annex
import Types.RepoSize
import Logs.Presence.Pure

import qualified Data.Map.Strict as M

updateRepoSize :: UUID -> Key -> LogStatus -> Annex ()
updateRepoSize u k s = Annex.getState Annex.reposizes >>= \case
	Nothing -> noop
	Just sizemap -> do
		let !sizemap' = M.adjust 
			(fromMaybe (RepoSize 0) . f k . Just)
			u sizemap
		Annex.changeState $ \st -> st
			{ Annex.reposizes = Just sizemap' }
  where
	f = case s of
		InfoPresent -> addKeyRepoSize
		InfoMissing -> removeKeyRepoSize
		InfoDead -> removeKeyRepoSize

addKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
addKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz + ksz
	Nothing -> Just $ RepoSize ksz
  where
	ksz = fromMaybe 0 $ fromKey keySize k

removeKeyRepoSize :: Key -> Maybe RepoSize -> Maybe RepoSize
removeKeyRepoSize k mrs = case mrs of
	Just (RepoSize sz) -> Just $ RepoSize $ sz - ksz
	Nothing -> Nothing
  where
	ksz = fromMaybe 0 $ fromKey keySize k
