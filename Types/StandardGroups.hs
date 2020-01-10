{- git-annex standard repository groups
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Types.StandardGroups where

import Types.Remote (RemoteConfig)
import Types.Group
import Types.ProposedAccepted

import qualified Data.Map as M

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

fromStandardGroup :: StandardGroup -> Group
fromStandardGroup ClientGroup = Group "client"
fromStandardGroup TransferGroup = Group "transfer"
fromStandardGroup BackupGroup = Group "backup"
fromStandardGroup IncrementalBackupGroup = Group "incrementalbackup"
fromStandardGroup SmallArchiveGroup = Group "smallarchive"
fromStandardGroup FullArchiveGroup = Group "archive"
fromStandardGroup SourceGroup = Group "source"
fromStandardGroup ManualGroup = Group "manual"
fromStandardGroup PublicGroup = Group "public"
fromStandardGroup UnwantedGroup = Group "unwanted"

toStandardGroup :: Group -> Maybe StandardGroup
toStandardGroup (Group "client") = Just ClientGroup
toStandardGroup (Group "transfer") = Just TransferGroup
toStandardGroup (Group "backup") = Just BackupGroup
toStandardGroup (Group "incrementalbackup") = Just IncrementalBackupGroup
toStandardGroup (Group "smallarchive") = Just SmallArchiveGroup
toStandardGroup (Group "archive") = Just FullArchiveGroup
toStandardGroup (Group "source") = Just SourceGroup
toStandardGroup (Group "manual") = Just ManualGroup
toStandardGroup (Group "public") = Just PublicGroup
toStandardGroup (Group "unwanted") = Just UnwantedGroup
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
	maybe "public" fromProposedAccepted $
		M.lookup (Accepted "preferreddir") c
associatedDirectory Nothing PublicGroup = Just "public"
associatedDirectory _ _ = Nothing

specialRemoteOnly :: StandardGroup -> Bool
specialRemoteOnly PublicGroup = True
specialRemoteOnly _ = False

{- See doc/preferred_content.mdwn for explanations of these expressions. -}
standardPreferredContent :: StandardGroup -> PreferredContentExpression
standardPreferredContent ClientGroup = lastResort $
	"include=* and ((exclude=*/archive/* and exclude=archive/*) or (" ++ notArchived ++ "))"
standardPreferredContent TransferGroup = lastResort $
	"not (inallgroup=client and copies=client:2) and (" ++ standardPreferredContent ClientGroup ++ ")"
standardPreferredContent BackupGroup = "anything"
standardPreferredContent IncrementalBackupGroup = lastResort
	"(not copies=backup:1) and (not copies=incrementalbackup:1)"
standardPreferredContent SmallArchiveGroup = lastResort $
	"(include=*/archive/* or include=archive/*) and (" ++ standardPreferredContent FullArchiveGroup ++ ")"
standardPreferredContent FullArchiveGroup = lastResort notArchived
standardPreferredContent SourceGroup = "not (copies=1)"
standardPreferredContent ManualGroup = "present and (" ++ standardPreferredContent ClientGroup ++ ")"
standardPreferredContent PublicGroup = "inpreferreddir"
standardPreferredContent UnwantedGroup = "not anything"

notArchived :: String
notArchived = "not (copies=archive:1 or copies=smallarchive:1)"
	
{- Most repositories want any content that is only on untrusted
 - or dead repositories, or that otherwise does not have enough copies.
 - Does not look at .gitattributes since that is quite a lot slower.
 -}
lastResort :: String -> PreferredContentExpression
lastResort s = "(" ++ s ++ ") or approxlackingcopies=1"
