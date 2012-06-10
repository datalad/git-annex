{-# LANGUAGE NamedFieldPuns #-}
{- cabal setup file -}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils (installOrdinaryFiles, rawSystemExit)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Verbosity (Verbosity)
import System.FilePath

import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks
	{ preConf = configure
	, postInst = myPostInst
	, postCopy = myPostCopy
	}

configure _ _ = do
	Configure.run Configure.tests
	return (Nothing, [])

myPostInst :: Args -> InstallFlags -> PackageDescription
           -> LocalBuildInfo -> IO ()
myPostInst _ (InstallFlags { installVerbosity }) pkg lbi = do
	installGitAnnexShell dest verbosity pkg lbi
	installManpages      dest verbosity pkg lbi
	where
		dest      = NoCopyDest
		verbosity = fromFlag installVerbosity

-- ???: Not sure how you're supposed to use this.  E.g., when I do
--
--    cabal install --prefix=/tmp/git-annex-install
--    cabal copy --deistdir=/tmp/git-annex-copy
--
-- I get the copy under
--
--   /tmp/git-annex-copy/tmp/git-annex-install
--
-- Also, `cabal install` fails when given a relative --prefix.
myPostCopy :: Args -> CopyFlags -> PackageDescription
           -> LocalBuildInfo -> IO ()
myPostCopy _ (CopyFlags { copyDest, copyVerbosity }) pkg lbi = do
	installGitAnnexShell dest verbosity pkg lbi
	installManpages      dest verbosity pkg lbi
	where
		dest      = fromFlag copyDest
		verbosity = fromFlag copyVerbosity

installGitAnnexShell :: CopyDest -> Verbosity -> PackageDescription
                     -> LocalBuildInfo -> IO ()
installGitAnnexShell copyDest verbosity pkg lbi =
	rawSystemExit verbosity "ln"
		["-sf", "git-annex", dstBinDir </> "git-annex-shell"]
	where
		dstBinDir = bindir $ absoluteInstallDirs pkg lbi copyDest

-- See http://www.haskell.org/haskellwiki/Cabal/Developer-FAQ#Installing_manpages.
--
-- Based on pandoc's and lhs2tex's 'Setup.installManpages' and
-- 'postInst' hooks.
--
-- My understanding: 'postCopy' is run for `cabal copy`, 'postInst' is
-- run for `cabal inst`, and copy is not a generalized install, so you
-- have to write two nearly identical hooks. 
--
-- Summary of hooks:
-- http://www.haskell.org/cabal/release/cabal-latest/doc/API/Cabal/Distribution-Simple-UserHooks.htm--
-- Other people are also confused:
--
-- * Bug: 'postCopy' and 'postInst' are confusing:
-- http://hackage.haskell.org/trac/hackage/ticket/718
--
-- * A cabal maintainer suggests using 'postCopy' instead of
-- 'postInst', because `cabal install` is `cabal copy` followed by
-- `cabal register`:
-- http://www.haskell.org/pipermail/libraries/2008-March/009416.html
-- Although that sounds desirable, it's not true, as the reply and
-- experiments indicate.
installManpages :: CopyDest -> Verbosity -> PackageDescription
                -> LocalBuildInfo -> IO ()
installManpages copyDest verbosity pkg lbi =
	installOrdinaryFiles verbosity dstManDir srcManpages
	where
		dstManDir   = mandir (absoluteInstallDirs pkg lbi copyDest) </> "man1"
		srcManpages = zip (repeat srcManDir) manpages
		srcManDir   = ""
		manpages    = ["git-annex.1", "git-annex-shell.1"]
