{-# LANGUAGE NamedFieldPuns #-}

{- cabal setup file -}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils (installOrdinaryFiles, rawSystemExit)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Verbosity (Verbosity)
import System.FilePath

import qualified Build.InstallDesktopFile as InstallDesktopFile
import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks
	{ preConf = configure
	, postInst = myPostInst
	}

configure _ _ = do
	Configure.run Configure.tests
	return (Nothing, [])

myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostInst _ (InstallFlags { installVerbosity }) pkg lbi = do
	installGitAnnexShell dest verbosity pkg lbi
	installManpages      dest verbosity pkg lbi
	installDesktopFile   dest verbosity pkg lbi
	where
		dest      = NoCopyDest
		verbosity = fromFlag installVerbosity

installGitAnnexShell :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installGitAnnexShell copyDest verbosity pkg lbi =
	rawSystemExit verbosity "ln"
		["-sf", "git-annex", dstBinDir </> "git-annex-shell"]
	where
		dstBinDir = bindir $ absoluteInstallDirs pkg lbi copyDest

-- See http://www.haskell.org/haskellwiki/Cabal/Developer-FAQ#Installing_manpages.
--
-- Based on pandoc's Setup.hs.
installManpages :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installManpages copyDest verbosity pkg lbi =
	installOrdinaryFiles verbosity dstManDir srcManpages
	where
		dstManDir   = mandir (absoluteInstallDirs pkg lbi copyDest) </> "man1"
		srcManpages = zip (repeat srcManDir) manpages
		srcManDir   = ""
		manpages    = ["git-annex.1", "git-annex-shell.1"]

installDesktopFile :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installDesktopFile copyDest verbosity pkg lbi =
	InstallDesktopFile.writeDesktop $ InstallDesktopFile.desktop $ dstBinDir </> "git-annex"
	where
		dstBinDir = bindir $ absoluteInstallDirs pkg lbi copyDest
