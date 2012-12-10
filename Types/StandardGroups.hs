{- git-annex standard repository groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.StandardGroups where

data StandardGroup = ClientGroup | TransferGroup | BackupGroup | SmallArchiveGroup | FullArchiveGroup
	deriving (Eq, Ord, Enum, Bounded, Show)

fromStandardGroup :: StandardGroup -> String
fromStandardGroup ClientGroup = "client"
fromStandardGroup TransferGroup = "transfer"
fromStandardGroup BackupGroup = "backup"
fromStandardGroup SmallArchiveGroup = "smallarchive"
fromStandardGroup FullArchiveGroup = "archive"

toStandardGroup :: String -> Maybe StandardGroup
toStandardGroup "client" = Just ClientGroup
toStandardGroup "transfer" = Just TransferGroup
toStandardGroup "backup" = Just BackupGroup
toStandardGroup "smallarchive" = Just SmallArchiveGroup
toStandardGroup "archive" = Just FullArchiveGroup
toStandardGroup _ = Nothing

descStandardGroup :: StandardGroup -> String
descStandardGroup ClientGroup = "client: a repository on your computer"
descStandardGroup TransferGroup = "transfer: distributes files to clients"
descStandardGroup BackupGroup = "backup: backs up all files"
descStandardGroup SmallArchiveGroup = "small archive: archives files located in \"archive\" directories"
descStandardGroup FullArchiveGroup = "full archive: archives all files not archived elsewhere"

{- See doc/preferred_content.mdwn for explanations of these expressions. -}
preferredContent :: StandardGroup -> String
preferredContent ClientGroup = "exclude=*/archive/* and exclude=archive/*"
preferredContent TransferGroup = "not (inallgroup=client and copies=client:2) and " ++ preferredContent ClientGroup
preferredContent BackupGroup = "include=*"
preferredContent SmallArchiveGroup = "(include=*/archive/* or include=archive/*) and " ++ preferredContent FullArchiveGroup
preferredContent FullArchiveGroup = "not (copies=archive:1 or copies=smallarchive:1)"
