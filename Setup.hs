{- cabal setup file -}

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Utils (installOrdinaryFiles)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Verbosity (Verbosity)
import System.Cmd
import System.FilePath

import qualified Build.Configure as Configure

main = defaultMainWithHooks simpleUserHooks
	{ preConf = configure
	, instHook = install
        , postInst = const installManpages
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

-- See http://www.haskell.org/haskellwiki/Cabal/Developer-FAQ#Installing_manpages.
--
-- Based on pandoc's 'Setup.installManpages' and 'postInst' hook.
-- Would be easier to just use 'rawSystem' as above.
--
-- XXX: lhs2tex installs man pages with the 'postCopy' hook. 
-- I chose 'postInst'.  Pandoc uses both :P  So, probably
-- to use the 'postCopy' hook.
--
-- XXX: fix tabs!
installManpages :: InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
installManpages flags pkg lbi =
  installOrdinaryFiles verbosity dstManDir manpages
  where
    srcManDir = ""
    -- The 'NoCopyDest' means "don't add an additional path prefix".
    -- The pandoc Setup.hs uses 'NoCopyDest' in the post install hook
    -- and the 'CopyDest' from the copy flags in the post copy hook.
    dstManDir = mandir (absoluteInstallDirs pkg lbi NoCopyDest) </> "man1"
    manpages  = zip (repeat srcManDir)
                    [ "git-annex.1"
                    , "git-annex-shell.1" ]
    verbosity = fromFlag $ installVerbosity flags
