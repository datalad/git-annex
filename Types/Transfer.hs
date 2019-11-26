{- git-annex transfer types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}

module Types.Transfer where

import Types
import Types.Remote (Verification(..))
import Types.Key
import Utility.PID
import Utility.QuickCheck
import Utility.Url
import Utility.FileSystemEncoding

import Data.Time.Clock.POSIX
import Control.Concurrent
import Control.Applicative
import Prelude

{- Enough information to uniquely identify a transfer. -}
data Transfer = Transfer
	{ transferDirection :: Direction
	, transferUUID :: UUID
	, transferKeyData :: KeyData
	}
	deriving (Eq, Ord, Show, Read)

transferKey :: Transfer -> Key
transferKey = mkKey . const . transferKeyData

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
	, associatedFile :: AssociatedFile
	, transferPaused :: Bool
	}
	deriving (Show, Eq, Ord)

stubTransferInfo :: TransferInfo
stubTransferInfo = TransferInfo Nothing Nothing Nothing Nothing Nothing (AssociatedFile Nothing) False

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
		<*> arbitrary
		<*> arbitrary

class Observable a where
	observeBool :: a -> Bool
	observeFailure :: a

instance Observable Bool where
	observeBool = id
	observeFailure = False

instance Observable (Bool, Verification) where
	observeBool = fst
	observeFailure = (False, UnVerified)

instance Observable (Either e Bool) where
	observeBool (Left _) = False
	observeBool (Right b) = b
	observeFailure = Right False

instance Observable (Maybe a) where
	observeBool (Just _) = True
	observeBool Nothing = False
	observeFailure = Nothing

class Transferrable t where
	descTransfrerrable :: t -> Maybe String

instance Transferrable AssociatedFile where
	descTransfrerrable (AssociatedFile af) = fromRawFilePath <$> af

instance Transferrable URLString where
	descTransfrerrable = Just
