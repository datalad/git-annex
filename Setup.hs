{- cabal setup file -}

import Distribution.Simple
import System.Cmd

import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks { preConf = configure }

configure _ _ = do
	Configure.run $ Configure.tests True
	return (Nothing, [])
