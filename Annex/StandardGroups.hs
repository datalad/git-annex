{- git-annex standard repository groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.StandardGroups where

import Common.Annex
import Logs.Group

import qualified Data.Set as S

data StandardGroup = ClientGroup | TransferGroup | ArchiveGroup | BackupGroup

fromStandardGroup :: StandardGroup -> String
fromStandardGroup ClientGroup = "client"
fromStandardGroup TransferGroup = "transfer"
fromStandardGroup ArchiveGroup = "archive"
fromStandardGroup BackupGroup = "backup"

toStandardGroup :: String -> Maybe StandardGroup
toStandardGroup "client" = Just ClientGroup
toStandardGroup "transfer" = Just TransferGroup
toStandardGroup "archive" = Just ArchiveGroup
toStandardGroup "backup" = Just BackupGroup
toStandardGroup _ = Nothing

{- See doc/preferred_content.mdwn for explanations of these expressions. -}
preferredContent :: StandardGroup -> String
preferredContent ClientGroup = "exclude=*/archive/*"
preferredContent TransferGroup = "not inallgroup=client and " ++ preferredContent ClientGroup
preferredContent ArchiveGroup = "not copies=archive:1"
preferredContent BackupGroup = "" -- all content is preferred

setStandardGroup :: UUID -> StandardGroup -> Annex ()
setStandardGroup u = groupSet u . S.singleton . fromStandardGroup
