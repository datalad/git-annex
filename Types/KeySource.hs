{- KeySource data type
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.KeySource where

{- When content is in the process of being added to the annex,
 - and a Key generated from it, this data type is used. 
 -
 - The contentLocation may be different from the filename
 - associated with the key. For example, the add command
 - temporarily puts the content into a lockdown directory
 - for checking. The migrate command uses the content
 - of a different Key. -}
data KeySource = KeySource
	{ keyFilename :: FilePath
	, contentLocation :: FilePath
	}
	deriving (Show)
