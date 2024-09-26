{- Parts of the Prelude are partial functions, which are a common source of
 - bugs.
 -
 - This exports functions that conflict with the prelude, which avoids
 - them being accidentally used.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.PartialPrelude (
	Utility.PartialPrelude.read,
	Utility.PartialPrelude.init,
	Utility.PartialPrelude.last,
	Utility.PartialPrelude.readish,
	Utility.PartialPrelude.headMaybe,
	Utility.PartialPrelude.lastMaybe,
	Utility.PartialPrelude.beginning,
	Utility.PartialPrelude.end,
) where

import qualified Data.Maybe

{- read should be avoided, as it throws an error
 - Instead, use: readish -}
read :: Read a => String -> a
read = Prelude.read

{- init too
 - Instead, use: beginning -}
init :: [a] -> [a]
init = Prelude.init

{- last too
 - Instead, use: end or lastMaybe -}
last :: [a] -> a
last = Prelude.last

{- Attempts to read a value from a String.
 -
 - Unlike Text.Read.readMaybe, this ignores some trailing text
 - after the part that can be read. However, if the trailing text looks
 - like another readable value, it fails.
 -}
readish :: Read a => String -> Maybe a
readish s = case reads s of
	((x,_):_) -> Just x
	_ -> Nothing

{- Like head but Nothing on empty list. -}
headMaybe :: [a] -> Maybe a
headMaybe = Data.Maybe.listToMaybe

{- Like last but Nothing on empty list. -}
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe v = Just $ Prelude.last v

{- All but the last element of a list.
 - (Like init, but no error on an empty list.) -}
beginning :: [a] -> [a]
beginning [] = []
beginning l = Prelude.init l

{- Like last, but no error on an empty list. -}
end :: [a] -> [a]
end [] = []
end l = [Prelude.last l]
