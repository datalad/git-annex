{- git-annex Messages data types
 - 
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Messages where

data OutputType = NormalOutput | QuietOutput | JSONOutput

data SideActionBlock = NoBlock | StartBlock | InBlock

data MessageState = MessageState
	{ outputType :: OutputType
	, sideActionBlock :: SideActionBlock
	}

defaultMessageState :: MessageState
defaultMessageState = MessageState NormalOutput NoBlock
