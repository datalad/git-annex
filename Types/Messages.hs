{- git-annex Messages data types
 - 
 - Copyright 2012-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.Messages where

import qualified Data.Aeson as Aeson

import Control.Concurrent
#ifdef WITH_CONCURRENTOUTPUT
import System.Console.Regions (ConsoleRegion)
#endif

data OutputType = NormalOutput | QuietOutput | JSONOutput JSONOptions
	deriving (Show)

data JSONOptions = JSONOptions
	{ jsonProgress :: Bool
	}
	deriving (Show)

adjustOutputType :: OutputType -> OutputType -> OutputType
adjustOutputType (JSONOutput old) (JSONOutput new) = JSONOutput $ JSONOptions
	{ jsonProgress = jsonProgress old || jsonProgress new
	}
adjustOutputType _old new = new

data SideActionBlock = NoBlock | StartBlock | InBlock
	deriving (Eq)

data MessageState = MessageState
	{ outputType :: OutputType
	, concurrentOutputEnabled :: Bool
	, sideActionBlock :: SideActionBlock
	, implicitMessages :: Bool
#ifdef WITH_CONCURRENTOUTPUT
	, consoleRegion :: Maybe ConsoleRegion
	, consoleRegionErrFlag :: Bool
#endif
	, jsonBuffer :: Maybe Aeson.Object
	, promptLock :: MVar () -- left full when not prompting
	}

newMessageState :: IO MessageState
newMessageState = do
	promptlock <- newMVar ()
	return $ MessageState
		{ outputType = NormalOutput
		, concurrentOutputEnabled = False
		, sideActionBlock = NoBlock
		, implicitMessages = True 
#ifdef WITH_CONCURRENTOUTPUT
		, consoleRegion = Nothing
		, consoleRegionErrFlag = False
#endif
		, jsonBuffer = Nothing
		, promptLock = promptlock
		}
