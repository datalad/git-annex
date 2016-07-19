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
import Data.Maybe
import Control.Exception
import qualified System.Info

import qualified Build.DesktopFile as DesktopFile
import qualified Build.Configure as Configure
import Build.Mans (buildMans)
import Utility.SafeCommand

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
	{ preConf = \_ _ -> do
		Configure.run Configure.tests
		return (Nothing, [])	
	, postCopy = myPostCopy
	}

myPostCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostCopy _ flags pkg lbi = when (System.Info.os /= "mingw32") $ do
	installGitAnnexShell dest verbosity pkg lbi
	installManpages      dest verbosity pkg lbi
	installDesktopFile   dest verbosity pkg lbi
  where
	dest      = fromFlag $ copyDest flags
	verbosity = fromFlag $ copyVerbosity flags

installGitAnnexShell :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installGitAnnexShell copyDest verbosity pkg lbi =
	rawSystemExit verbosity "ln"
		["-sf", "git-annex", dstBinDir </> "git-annex-shell"]
  where
	dstBinDir = bindir $ absoluteInstallDirs pkg lbi copyDest

{- See http://www.haskell.org/haskellwiki/Cabal/Developer-FAQ#Installing_manpages -}
installManpages :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installManpages copyDest verbosity pkg lbi =
	installOrdinaryFiles verbosity dstManDir =<< srcManpages
  where
	dstManDir   = mandir (absoluteInstallDirs pkg lbi copyDest) </> "man1"
	-- If mdwn2man fails, perhaps because perl is not available,
	-- we just skip installing man pages.
	srcManpages = zip (repeat "man") . map takeFileName . catMaybes
		<$> buildMans

installDesktopFile :: CopyDest -> Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
installDesktopFile copyDest _verbosity pkg lbi
	| progfile copyDest == progfile NoCopyDest =
		DesktopFile.installUser (progfile copyDest)
			`catch` installerror
	| otherwise = return ()
  where
	progfile cd = bindir (absoluteInstallDirs pkg lbi cd) </> "git-annex"
	installerror :: SomeException -> IO ()
	installerror e = putStrLn ("Warning: Installation of desktop integration files did not succeed (" ++ show e ++ "); skipping.")
