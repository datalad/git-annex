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
wantGet :: Bool -> AssociatedFile -> Annex Bool
wantGet def Nothing = return def
wantGet def (Just file) = isPreferredContent Nothing S.empty file def

{- Check if a file is preferred content for a remote. -}
wantSend :: Bool -> AssociatedFile -> UUID -> Annex Bool
wantSend def Nothing _ = return def
wantSend def (Just file) to = isPreferredContent (Just to) S.empty file def

{- Check if a file can be dropped, maybe from a remote.
 - Don't drop files that are preferred content. -}
wantDrop :: Bool -> Maybe UUID -> AssociatedFile -> Annex Bool
wantDrop def _ Nothing = return $ not def
wantDrop def from (Just file) = do
	u <- maybe getUUID (return . id) from
	not <$> isPreferredContent (Just u) (S.singleton u) file def
