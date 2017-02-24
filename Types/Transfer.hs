{- git-annex transfer types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Transfer where

import Types
import Utility.PID
import Utility.QuickCheck

import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Applicative
import Prelude

{- Enough information to uniquely identify a transfer. -}
data Transfer = Transfer
	{ transferDirection :: Direction
	, transferUUID :: UUID
	, transferKey :: Key
	}
	deriving (Eq, Ord, Read, Show)

{- Information about a Transfer, stored in the transfer information file.
 -
 - Note that the associatedFile may not correspond to a file in the local
 - git repository. It's some file, possibly relative to some directory,
 - of some repository, that was acted on to initiate the transfer.
 -}
data TransferInfo = TransferInfo
	{ startedTime :: Maybe POSIXTime
	, transferPid :: Maybe PID
	, transferTid :: Maybe ThreadId
	, transferRemote :: Maybe Remote
	, bytesComplete :: Maybe Integer
	, associatedFile :: Maybe FilePath
	, transferPaused :: Bool
	}
	deriving (Show, Eq, Ord)

stubTransferInfo :: TransferInfo
stubTransferInfo = TransferInfo Nothing Nothing Nothing Nothing Nothing Nothing False

data Direction = Upload | Download
	deriving (Eq, Ord, Show, Read)

formatDirection :: Direction -> String
formatDirection Upload = "upload"
formatDirection Download = "download"

parseDirection :: String -> Maybe Direction
parseDirection "upload" = Just Upload
parseDirection "download" = Just Download
parseDirection _ = Nothing

instance Arbitrary TransferInfo where
	arbitrary = TransferInfo
		<$> arbitrary
		<*> arbitrary
		<*> pure Nothing -- cannot generate a ThreadID
		<*> pure Nothing -- remote not needed
		<*> arbitrary
		-- associated file cannot be empty (but can be Nothing)
		<*> arbitrary `suchThat` (/= Just "")
		<*> arbitrary
