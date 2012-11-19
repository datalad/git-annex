{- git-annex meter types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Meters where

{- An action that can be run repeatedly, feeding it the number of
 - bytes sent or retrieved so far. -}
type MeterUpdate = (Integer -> IO ())
