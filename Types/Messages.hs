{- git-annex Messages data types
 - 
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Messages where

import Data.Default

data OutputType = NormalOutput | QuietOutput | ConcurrentOutput Int | JSONOutput

data SideActionBlock = NoBlock | StartBlock | InBlock
	deriving (Eq)

data MessageState = MessageState
	{ outputType :: OutputType
	, sideActionBlock :: SideActionBlock
	}

instance Default MessageState
  where
	def = MessageState NormalOutput NoBlock
