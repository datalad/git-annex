{- git-annex Messages data types
 - 
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Messages where

import qualified Utility.Aeson as Aeson
import Utility.Metered

import Control.Concurrent
import System.Console.Regions (ConsoleRegion)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

data OutputType
	= NormalOutput
	| QuietOutput
	| JSONOutput JSONOptions
	| SerializedOutput
	deriving (Show)

data JSONOptions = JSONOptions
	{ jsonProgress :: Bool
	, jsonErrorMessages :: Bool
	}
	deriving (Show)

adjustOutputType :: OutputType -> OutputType -> OutputType
adjustOutputType (JSONOutput old) (JSONOutput new) = JSONOutput $ JSONOptions
	{ jsonProgress = jsonProgress old || jsonProgress new
	, jsonErrorMessages = jsonErrorMessages old || jsonErrorMessages new
	}
adjustOutputType _old new = new

data SideActionBlock = NoBlock | StartBlock | InBlock
	deriving (Eq)

data MessageState = MessageState
	{ outputType :: OutputType
	, concurrentOutputEnabled :: Bool
	, sideActionBlock :: SideActionBlock
	, consoleRegion :: Maybe ConsoleRegion
	, consoleRegionErrFlag :: Bool
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
		, consoleRegion = Nothing
		, consoleRegionErrFlag = False
		, jsonBuffer = Nothing
		, promptLock = promptlock
		}

data SerializedOutput
	= OutputMessage S.ByteString
	| OutputError String
	| ProgressMeter (Maybe Integer) MeterState MeterState
	| JSONObject L.ByteString
	deriving (Show, Read)
