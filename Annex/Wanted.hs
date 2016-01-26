{- git-annex checking whether content is wanted
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Wanted where

import Annex.Common
import Logs.PreferredContent
import Annex.UUID

import qualified Data.Set as S

{- Check if a file is preferred content for the local repository. -}
wantGet :: Bool -> Maybe Key -> AssociatedFile -> Annex Bool
wantGet d key file = isPreferredContent Nothing S.empty key file d

{- Check if a file is preferred content for a remote. -}
wantSend :: Bool -> Maybe Key -> AssociatedFile -> UUID -> Annex Bool
wantSend d key file to = isPreferredContent (Just to) S.empty key file d

{- Check if a file can be dropped, maybe from a remote.
 - Don't drop files that are preferred content. -}
wantDrop :: Bool -> Maybe UUID -> Maybe Key -> AssociatedFile -> Annex Bool
wantDrop d from key file = do
	u <- maybe getUUID (return . id) from
	not <$> isPreferredContent (Just u) (S.singleton u) key file d
