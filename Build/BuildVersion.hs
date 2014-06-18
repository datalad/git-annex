{- Outputs the version of git-annex that was built, for use by
 - autobuilders. Note that this includes the git rev. -}

import Build.Version

main = putStr =<< getVersion
