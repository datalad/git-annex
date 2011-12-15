{- Some stuff from Prelude should not be used, as it tends to be a source
 - of bugs.
 -
 - This exports functions that conflict with the prelude, which avoids
 - them being accidentially used.
 -}

module Utility.BadPrelude where

{- read should be avoided, as it throws an error -}
read :: Read a => String -> a
read = Prelude.read

{- head is a partial function; head [] is an error
 - Instead, use: take 1 or headMaybe -}
head :: [a] -> a
head = Prelude.head

{- tail is also partial
 - Instead, use: drop 1 -}
tail :: [a] -> [a]
tail = Prelude.tail

{- init too
 - Instead, use: beginning -}
init :: [a] -> [a]
init = Prelude.init

{- last too
 - Instead, use: end or lastMaybe -}
last :: [a] -> a
last = Prelude.last

{- Like head but Nothing on empty list. -}
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe v = Just $ Prelude.head v

{- Like last but Nothing on empty list. -}
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe v = Just $ Prelude.last v

{- All but the last element of a list.
 - (Like init, but no error on an empty list.) -}
beginning :: [a] -> [a]
beginning [] = []
beginning (x:xs) =  beginning' x xs
	where
		beginning' _ [] = []
		beginning' y (z:zs) = y : beginning' z zs

{- Like last, but no error on an empty list. -}
end :: [a] -> [a]
end [] = []
end (x:xs) = end' x xs
	where
		end' y [] = [y]
		end' _ (y:ys) = end' y ys

