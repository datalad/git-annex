{- cabal setup file -}

import Distribution.Simple
import System.Cmd

main = defaultMainWithHooks simpleUserHooks {
	preConf = makeSources,
	postClean = makeClean
}

makeSources _ _ = do
	system "make sources"
	return (Nothing, [])

makeClean _ _ _ _ = do
	system "make clean"
	return ()
