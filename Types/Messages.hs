{- git-annex Messages data types
 - 
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Messages where

import qualified Data.Set as S

data OutputType = NormalOutput | QuietOutput | JSONOutput

data SideActionBlock = NoBlock | StartBlock | InBlock
	deriving (Eq)

data MessageState = MessageState
	{ outputType :: OutputType
	, sideActionBlock :: SideActionBlock
	, fileNotFoundShown :: S.Set FilePath
	}

defaultMessageState :: MessageState
defaultMessageState = MessageState NormalOutput NoBlock S.empty
