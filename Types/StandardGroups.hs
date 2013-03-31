{- git-annex standard repository groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.StandardGroups where

data StandardGroup
	= ClientGroup
	| TransferGroup
	| BackupGroup
	| IncrementalBackupGroup
	| SmallArchiveGroup
	| FullArchiveGroup
	| SourceGroup
	| ManualGroup
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
descStandardGroup UnwantedGroup "unwanted: a repository in the process of being removed"

{- See doc/preferred_content.mdwn for explanations of these expressions. -}
preferredContent :: StandardGroup -> String
preferredContent ClientGroup = normal
	"exclude=*/archive/* and exclude=archive/*"
preferredContent TransferGroup = normal $
	"not (inallgroup=client and copies=client:2) and " ++ preferredContent ClientGroup
preferredContent BackupGroup = "include=*"
preferredContent IncrementalBackupGroup = normal $
	"include=* and (not copies=incrementalbackup:1)"
preferredContent SmallArchiveGroup = normal $
	"(include=*/archive/* or include=archive/*) and " ++ preferredContent FullArchiveGroup
preferredContent FullArchiveGroup = normal $
	"not (copies=archive:1 or copies=smallarchive:1)"
preferredContent SourceGroup = "not (copies=1)"
preferredContent ManualGroup = normal $
	"present and exclude=*/archive/* and exclude=archive/*"
preferredContent UnwantedGroup = "exclude=*"
  where
  	{- Most repositories want any content that is only on untrusted
	 - or dead repositories. -}
	normal s = "(" ++ s ++ ") or (not copies=semitrusted:1)"
