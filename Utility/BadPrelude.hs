{- Some stuff from Prelude should not be used, as it tends to be a source
 - of bugs.
 -
 - This exports functions that conflict with the prelude, which avoids
 - them being accidentially used.
 -}

module Utility.BadPrelude where

{- head is a partial function; head [] is an error -}
head :: [a] -> a
head = Prelude.head

{- tail is also partial -}
tail :: [a] -> a
tail = Prelude.tail

{- init too -}
init :: [a] -> a
init = Prelude.init

{- last too -}
last :: [a] -> a
last = Prelude.last

{- read should be avoided, as it throws an error -}
read :: Read a => String -> a
read = Prelude.read
