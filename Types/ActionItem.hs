{- items that a command can act on
 -
 - Copyright 2016-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.ActionItem where

import Key
import Types.Transfer
import Git.FilePath
import qualified Git.Filename
import Utility.FileSystemEncoding

import Data.Maybe
import qualified Data.ByteString as S

data ActionItem 
	= ActionItemAssociatedFile AssociatedFile Key
	| ActionItemKey Key
	| ActionItemBranchFilePath BranchFilePath Key
	| ActionItemFailedTransfer Transfer TransferInfo
	| ActionItemTreeFile RawFilePath
	| ActionItemOther (Maybe String)
	-- Use to avoid more than one thread concurrently processing the
	-- same Key.
	| OnlyActionOn Key ActionItem
	deriving (Show, Eq)

class MkActionItem t where
	mkActionItem :: t -> ActionItem

instance MkActionItem ActionItem where
	mkActionItem = id

instance MkActionItem (AssociatedFile, Key) where
	mkActionItem = uncurry ActionItemAssociatedFile

instance MkActionItem (Key, AssociatedFile) where
	mkActionItem = uncurry $ flip ActionItemAssociatedFile

instance MkActionItem (Key, RawFilePath) where
	mkActionItem (key, file) = ActionItemAssociatedFile (AssociatedFile (Just file)) key

instance MkActionItem (RawFilePath, Key) where
	mkActionItem (file, key) = mkActionItem (key, file)

instance MkActionItem Key where
	mkActionItem = ActionItemKey

instance MkActionItem (BranchFilePath, Key) where
	mkActionItem = uncurry ActionItemBranchFilePath

instance MkActionItem (Transfer, TransferInfo) where
	mkActionItem = uncurry ActionItemFailedTransfer

actionItemDesc :: Git.Filename.QuotePath -> ActionItem -> S.ByteString
actionItemDesc qp (ActionItemAssociatedFile (AssociatedFile (Just f)) _) = 
	Git.Filename.encode qp f
actionItemDesc _ (ActionItemAssociatedFile (AssociatedFile Nothing) k) = 
	serializeKey' k
actionItemDesc _ (ActionItemKey k) = serializeKey' k
actionItemDesc qp (ActionItemBranchFilePath bfp _) = descBranchFilePath qp bfp
actionItemDesc qp (ActionItemFailedTransfer t i) = actionItemDesc qp $
	ActionItemAssociatedFile (associatedFile i) (transferKey t)
actionItemDesc qp (ActionItemTreeFile f) = Git.Filename.encode qp f
actionItemDesc _ (ActionItemOther s) = encodeBS (fromMaybe "" s)
actionItemDesc qp (OnlyActionOn _ ai) = actionItemDesc qp ai

actionItemKey :: ActionItem -> Maybe Key
actionItemKey (ActionItemAssociatedFile _ k) = Just k
actionItemKey (ActionItemKey k) = Just k
actionItemKey (ActionItemBranchFilePath _ k) = Just k
actionItemKey (ActionItemFailedTransfer t _) = Just (transferKey t)
actionItemKey (ActionItemTreeFile _) = Nothing
actionItemKey (ActionItemOther _) = Nothing
actionItemKey (OnlyActionOn _ ai) = actionItemKey ai

actionItemFile :: ActionItem -> Maybe RawFilePath
actionItemFile (ActionItemAssociatedFile (AssociatedFile af) _) = af
actionItemFile (ActionItemTreeFile f) = Just f
actionItemFile (OnlyActionOn _ ai) = actionItemFile ai
actionItemFile _ = Nothing

actionItemTransferDirection :: ActionItem -> Maybe Direction
actionItemTransferDirection (ActionItemFailedTransfer t _) = Just $
	transferDirection t
actionItemTransferDirection (OnlyActionOn _ ai) = actionItemTransferDirection ai
actionItemTransferDirection _ = Nothing
