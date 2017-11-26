{- KeySource data type
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.KeySource where

import Utility.InodeCache

{- When content is in the process of being ingested into the annex,
 - and a Key generated from it, this data type is used. 
 -
 - The contentLocation may be different from the filename
 - associated with the key. For example, the add command
 - may temporarily hard link the content into a lockdown directory
 - for checking. The migrate command uses the content
 - of a different Key.
 -
 - The inodeCache can be used to detect some types of modifications to
 - files that may be made while they're in the process of being ingested.
 -}
data KeySource = KeySource
	{ keyFilename :: FilePath
	, contentLocation :: FilePath
	, inodeCache :: Maybe InodeCache
	}
	deriving (Show)
