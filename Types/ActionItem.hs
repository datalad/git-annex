{- items that a command can act on
 -
 - Copyright 2016-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.ActionItem (
	module Types.ActionItem,
	StringContainingQuotedPath(..),
) where

import Key
import Types.Transfer
import Git.FilePath
import Git.Filename (StringContainingQuotedPath(..))
import Utility.FileSystemEncoding

data ActionItem 
	= ActionItemAssociatedFile AssociatedFile Key
	| ActionItemKey Key
	| ActionItemBranchFilePath BranchFilePath Key
	| ActionItemFailedTransfer Transfer TransferInfo
	| ActionItemTreeFile RawFilePath
	| ActionItemOther (Maybe StringContainingQuotedPath)
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

actionItemDesc :: ActionItem -> StringContainingQuotedPath
actionItemDesc (ActionItemAssociatedFile (AssociatedFile (Just f)) _) = 
	QuotedPath f
actionItemDesc (ActionItemAssociatedFile (AssociatedFile Nothing) k) = 
	UnquotedByteString (serializeKey' k)
actionItemDesc (ActionItemKey k) =
	UnquotedByteString (serializeKey' k)
actionItemDesc (ActionItemBranchFilePath bfp _) =
	descBranchFilePath bfp
actionItemDesc (ActionItemFailedTransfer t i) = actionItemDesc $
	ActionItemAssociatedFile (associatedFile i) (transferKey t)
actionItemDesc (ActionItemTreeFile f) = QuotedPath f
actionItemDesc (ActionItemOther Nothing) = mempty
actionItemDesc (ActionItemOther (Just v)) = v
actionItemDesc (OnlyActionOn _ ai) = actionItemDesc ai

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
