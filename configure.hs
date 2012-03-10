{- configure program -}

import Data.Maybe

import qualified Build.Configure as Configure
import Build.TestConfig
import Utility.StatFS

tests :: [TestCase]
tests = [ TestCase "StatFS" testStatFS
	] ++ Configure.tests

{- This test cannot be included in Build.Configure due to needing
 - Utility/StatFS.hs to be built. -}
testStatFS :: Test
testStatFS = do
	s <- getFileSystemStats "."
	return $ Config "statfs_sane" $ BoolConfig $ isJust s

main :: IO ()
main = Configure.run tests
