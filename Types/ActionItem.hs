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

import Data.Maybe

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
actionItemDesc (ActionItemAssociatedFile (Just f)) _ = f
actionItemDesc (ActionItemAssociatedFile Nothing) k = key2file k
actionItemDesc ActionItemKey k = key2file k
actionItemDesc (ActionItemBranchFilePath bfp) _ = descBranchFilePath bfp
actionItemDesc (ActionItemFailedTransfer _ i) k = 
	fromMaybe (key2file k) (associatedFile i)

actionItemWorkTreeFile :: ActionItem -> Maybe FilePath
actionItemWorkTreeFile (ActionItemAssociatedFile af) = af
actionItemWorkTreeFile _ = Nothing

actionItemTransferDirection :: ActionItem -> Maybe Direction
actionItemTransferDirection (ActionItemFailedTransfer t _) = Just $
	transferDirection t
actionItemTransferDirection _ = Nothing
