{- items that a command can act on
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types.ActionItem where

import Key
import Types.Transfer
import Git.FilePath

data ActionItem 
	= ActionItemAssociatedFile AssociatedFile
	| ActionItemKey
	| ActionItemBranchFilePath BranchFilePath
	| ActionItemFailedTransfer Transfer TransferInfo

class MkActionItem t where
	mkActionItem :: t -> ActionItem

instance MkActionItem AssociatedFile where
	mkActionItem = ActionItemAssociatedFile

instance MkActionItem Key where
	mkActionItem _ = ActionItemKey

instance MkActionItem BranchFilePath where
	mkActionItem = ActionItemBranchFilePath

instance MkActionItem (Transfer, TransferInfo) where
	mkActionItem = uncurry ActionItemFailedTransfer

actionItemDesc :: ActionItem -> Key -> String
actionItemDesc (ActionItemAssociatedFile (AssociatedFile (Just f))) _ = f
actionItemDesc (ActionItemAssociatedFile (AssociatedFile Nothing)) k = serializeKey k
actionItemDesc ActionItemKey k = serializeKey k
actionItemDesc (ActionItemBranchFilePath bfp) _ = descBranchFilePath bfp
actionItemDesc (ActionItemFailedTransfer _ i) k =
	actionItemDesc (ActionItemAssociatedFile (associatedFile i)) k

actionItemWorkTreeFile :: ActionItem -> Maybe FilePath
actionItemWorkTreeFile (ActionItemAssociatedFile (AssociatedFile af)) = af
actionItemWorkTreeFile _ = Nothing

actionItemTransferDirection :: ActionItem -> Maybe Direction
actionItemTransferDirection (ActionItemFailedTransfer t _) = Just $
	transferDirection t
actionItemTransferDirection _ = Nothing
