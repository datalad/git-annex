{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{- cabal setup file -}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils (installOrdinaryFiles, rawSystemExit)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Verbosity (Verbosity)
import System.FilePath
import Control.Applicative
import Control.Monad
import System.Directory
import Data.List

import qualified Build.DesktopFile as DesktopFile
import qualified Build.Configure as Configure

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
	{ preConf = \_ _ -> do
		Configure.run Configure.tests
		return (Nothing, [])	
	, postInst = myPostInst
	}

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

{- See http://www.haskell.org/haskellwiki/Cabal/Developer-FAQ#Installing_manpages
 -
 - Man pages are provided prebuilt in the tarball in cabal,
 - but may not be available otherwise, in which case, skip installing them.
 -}
installManpages :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installManpages copyDest verbosity pkg lbi =
	installOrdinaryFiles verbosity dstManDir =<< srcManpages
  where
	dstManDir   = mandir (absoluteInstallDirs pkg lbi copyDest) </> "man1"
	srcManpages = do
		havemans <- doesDirectoryExist srcManDir
		if havemans
			then zip (repeat srcManDir)
				. filter (".1" `isSuffixOf`)
				<$> getDirectoryContents srcManDir
			else return []
	srcManDir   = "man"

installDesktopFile :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installDesktopFile copyDest _verbosity pkg lbi =
	DesktopFile.install $ dstBinDir </> "git-annex"
  where
	dstBinDir = bindir $ absoluteInstallDirs pkg lbi copyDest
