{- cabal setup file -}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Cmd
import System.FilePath

import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks
	{ preConf = configure
	, instHook = install
	}

configure _ _ = do
	Configure.run Configure.tests
	return (Nothing, [])

install pkg_descr lbi userhooks flags = do
	r <- (instHook simpleUserHooks) pkg_descr lbi userhooks flags
	_ <- rawSystem "ln" ["-sf", "git-annex", 
		bindir installDirs </> "git-annex-shell"]
	return r
	where
		installDirs = absoluteInstallDirs pkg_descr lbi $
			fromFlag (copyDest defaultCopyFlags)
