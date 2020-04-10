{- Alternative git index files
 - 
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.IndexFiles where

data AltIndexFile = AnnexIndexFile | ViewIndexFile
	deriving (Eq, Show)
