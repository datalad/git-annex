{- name of a thread
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.ThreadName where

newtype ThreadName = ThreadName String
	deriving (Eq, Read, Show, Ord)

fromThreadName :: ThreadName -> String
fromThreadName (ThreadName n) = n
