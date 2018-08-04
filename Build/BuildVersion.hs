{- For use by autobuilders, this outputs the version of git-annex that
 - is being built, and also updates the Build/Version file so the build
 - will report the right version, including the current git rev. -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

import Build.Version

main :: IO ()
main = do
	ver <- getVersion
	writeVersion ver
	putStr ver
