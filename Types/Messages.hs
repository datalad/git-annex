{- git-annex Messages data types
 - 
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Messages where

import Data.Default

#ifdef WITH_CONCURRENTOUTPUT
import System.Console.Regions (ConsoleRegion)
#endif

data OutputType = NormalOutput | QuietOutput | ConcurrentOutput Int | JSONOutput
	deriving (Show)

data SideActionBlock = NoBlock | StartBlock | InBlock
	deriving (Eq)

data MessageState = MessageState
	{ outputType :: OutputType
	, sideActionBlock :: SideActionBlock
	, consoleRegion :: Maybe ConsoleRegion
	, consoleRegionErrFlag :: Bool
	}

instance Default MessageState
  where
	def = MessageState NormalOutput NoBlock Nothing False
