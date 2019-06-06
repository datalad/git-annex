{- items that a command can act on
 -
 - Copyright 2016-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.ActionItem where

import Key
import Types.Transfer
import Git.FilePath

data ActionItem 
	= ActionItemAssociatedFile AssociatedFile Key
	| ActionItemKey Key
	| ActionItemBranchFilePath BranchFilePath Key
	| ActionItemFailedTransfer Transfer TransferInfo

class MkActionItem t where
	mkActionItem :: t -> ActionItem

instance MkActionItem (AssociatedFile, Key) where
	mkActionItem = uncurry ActionItemAssociatedFile

instance MkActionItem (Key, AssociatedFile) where
	mkActionItem = uncurry $ flip ActionItemAssociatedFile

instance MkActionItem Key where
	mkActionItem = ActionItemKey

instance MkActionItem (BranchFilePath, Key) where
	mkActionItem = uncurry ActionItemBranchFilePath

instance MkActionItem (Transfer, TransferInfo) where
	mkActionItem = uncurry ActionItemFailedTransfer

actionItemDesc :: ActionItem -> String
actionItemDesc (ActionItemAssociatedFile (AssociatedFile (Just f)) _) = f
actionItemDesc (ActionItemAssociatedFile (AssociatedFile Nothing) k) = serializeKey k
actionItemDesc (ActionItemKey k) = serializeKey k
actionItemDesc (ActionItemBranchFilePath bfp _) = descBranchFilePath bfp
actionItemDesc (ActionItemFailedTransfer t i) = actionItemDesc $
	ActionItemAssociatedFile (associatedFile i) (transferKey t)

actionItemKey :: ActionItem -> Key
actionItemKey (ActionItemAssociatedFile _ k) = k
actionItemKey (ActionItemKey k) = k
actionItemKey (ActionItemBranchFilePath _ k) = k
actionItemKey (ActionItemFailedTransfer t _) = transferKey t

actionItemWorkTreeFile :: ActionItem -> Maybe FilePath
actionItemWorkTreeFile (ActionItemAssociatedFile (AssociatedFile af) _) = af
actionItemWorkTreeFile _ = Nothing

actionItemTransferDirection :: ActionItem -> Maybe Direction
actionItemTransferDirection (ActionItemFailedTransfer t _) = Just $
	transferDirection t
actionItemTransferDirection _ = Nothing
