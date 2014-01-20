{- git-annex standard repository groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.StandardGroups where

import Types.Remote (RemoteConfig)

import qualified Data.Map as M
import Data.Maybe

type PreferredContentExpression = String

data StandardGroup
	= ClientGroup
	| TransferGroup
	| BackupGroup
	| IncrementalBackupGroup
	| SmallArchiveGroup
	| FullArchiveGroup
	| SourceGroup
	| ManualGroup
	| PublicGroup
	| UnwantedGroup
	deriving (Eq, Ord, Enum, Bounded, Show)

fromStandardGroup :: StandardGroup -> String
fromStandardGroup ClientGroup = "client"
fromStandardGroup TransferGroup = "transfer"
fromStandardGroup BackupGroup = "backup"
fromStandardGroup IncrementalBackupGroup = "incrementalbackup"
fromStandardGroup SmallArchiveGroup = "smallarchive"
fromStandardGroup FullArchiveGroup = "archive"
fromStandardGroup SourceGroup = "source"
fromStandardGroup ManualGroup = "manual"
fromStandardGroup PublicGroup = "public"
fromStandardGroup UnwantedGroup = "unwanted"

toStandardGroup :: String -> Maybe StandardGroup
toStandardGroup "client" = Just ClientGroup
toStandardGroup "transfer" = Just TransferGroup
toStandardGroup "backup" = Just BackupGroup
toStandardGroup "incrementalbackup" = Just IncrementalBackupGroup
toStandardGroup "smallarchive" = Just SmallArchiveGroup
toStandardGroup "archive" = Just FullArchiveGroup
toStandardGroup "source" = Just SourceGroup
toStandardGroup "manual" = Just ManualGroup
toStandardGroup "public" = Just PublicGroup
toStandardGroup "unwanted" = Just UnwantedGroup
toStandardGroup _ = Nothing

descStandardGroup :: StandardGroup -> String
descStandardGroup ClientGroup = "client: a repository on your computer"
descStandardGroup TransferGroup = "transfer: distributes files to clients"
descStandardGroup BackupGroup = "full backup: backs up all files"
descStandardGroup IncrementalBackupGroup = "incremental backup: backs up files not backed up elsewhere"
descStandardGroup SmallArchiveGroup = "small archive: archives files located in \"archive\" directories"
descStandardGroup FullArchiveGroup = "full archive: archives all files not archived elsewhere"
descStandardGroup SourceGroup = "file source: moves files on to other repositories"
descStandardGroup ManualGroup = "manual mode: only stores files you manually choose"
descStandardGroup UnwantedGroup = "unwanted: remove content from this repository"
descStandardGroup PublicGroup = "public: publishes files located in an associated directory"

associatedDirectory :: Maybe RemoteConfig -> StandardGroup -> Maybe FilePath
associatedDirectory _ SmallArchiveGroup = Just "archive"
associatedDirectory _ FullArchiveGroup = Just "archive"
associatedDirectory (Just c) PublicGroup = Just $
	fromMaybe "public" $ M.lookup "preferreddir" c
associatedDirectory Nothing PublicGroup = Just "public"
associatedDirectory _ _ = Nothing

{- See doc/preferred_content.mdwn for explanations of these expressions. -}
preferredContent :: StandardGroup -> PreferredContentExpression
preferredContent ClientGroup = lastResort $
	"(exclude=*/archive/* and exclude=archive/*) or (" ++ notArchived ++ ")"
preferredContent TransferGroup = lastResort $
	"not (inallgroup=client and copies=client:2) and (" ++ preferredContent ClientGroup ++ ")"
preferredContent BackupGroup = "include=*"
preferredContent IncrementalBackupGroup = lastResort
	"include=* and (not copies=incrementalbackup:1)"
preferredContent SmallArchiveGroup = lastResort $
	"(include=*/archive/* or include=archive/*) and (" ++ preferredContent FullArchiveGroup ++ ")"
preferredContent FullArchiveGroup = lastResort notArchived
preferredContent SourceGroup = "not (copies=1)"
preferredContent ManualGroup = "present and (" ++ preferredContent ClientGroup ++ ")"
preferredContent PublicGroup = "inpreferreddir"
preferredContent UnwantedGroup = "exclude=*"

notArchived :: String
notArchived = "not (copies=archive:1 or copies=smallarchive:1)"
  	
{- Most repositories want any content that is only on untrusted
 - or dead repositories, or that otherwise does not have enough copies. -}
lastResort :: String -> PreferredContentExpression
lastResort s = "(" ++ s ++ ") or numcopiesneeded=1"
