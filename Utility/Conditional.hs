{- monadic conditional operators
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Conditional where

import Control.Monad (when, unless)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c a = c >>= flip when a

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= flip unless a

(>>?) :: Monad m => m Bool -> m () -> m ()
(>>?) = whenM

(>>!) :: Monad m => m Bool -> m () -> m ()
(>>!) = unlessM

-- low fixity allows eg, foo bar >>! error $ "failed " ++ meep
infixr 0 >>?
infixr 0 >>!
