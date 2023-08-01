{-# OPTIONS_GHC -fno-warn-tabs #-}

{- cabal setup file -}

import Distribution.Simple
import qualified Build.Configure as Configure

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
	{ preConf = \_ _ -> do
		Configure.run Configure.tests
		return (Nothing, [])	
	}
