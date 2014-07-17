{- git-annex checking whether content is wanted
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Wanted where

import Common.Annex
import Logs.PreferredContent
import Annex.UUID

import qualified Data.Set as S

{- Check if a file is preferred content for the local repository. -}
wantGet :: Bool -> Maybe Key -> AssociatedFile -> Annex Bool
wantGet def key file = isPreferredContent Nothing S.empty key file def

{- Check if a file is preferred content for a remote. -}
wantSend :: Bool -> Maybe Key -> AssociatedFile -> UUID -> Annex Bool
wantSend def key file to = isPreferredContent (Just to) S.empty key file def

{- Check if a file can be dropped, maybe from a remote.
 - Don't drop files that are preferred content. -}
wantDrop :: Bool -> Maybe UUID -> Maybe Key -> AssociatedFile -> Annex Bool
wantDrop def from key file = do
	u <- maybe getUUID (return . id) from
	not <$> isPreferredContent (Just u) (S.singleton u) key file def
