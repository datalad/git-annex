{- Keeping track of which tables in the keys database have changed
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Database.Keys.Tables where

data DbTable = AssociatedTable | ContentTable
	deriving (Eq, Show)

data DbTablesChanged = DbTablesChanged
	{ associatedTable :: Bool
	, contentTable :: Bool
	}
	deriving (Show)

instance Semigroup DbTablesChanged where
	a <> b = DbTablesChanged
		{ associatedTable = associatedTable a || associatedTable b
		, contentTable = contentTable a || contentTable b
		}

instance Monoid DbTablesChanged where
	mempty = DbTablesChanged False False

addDbTable :: DbTablesChanged -> DbTable -> DbTablesChanged
addDbTable ts AssociatedTable = ts { associatedTable = True }
addDbTable ts ContentTable = ts { contentTable = True }

isDbTableChanged :: DbTablesChanged -> DbTable -> Bool
isDbTableChanged ts AssociatedTable = associatedTable ts
isDbTableChanged ts ContentTable = contentTable ts
