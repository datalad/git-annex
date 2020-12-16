{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Version where

import Command
import Annex.Version
import BuildInfo
import BuildFlags
import Types.Key
import Types.RepoVersion
import qualified Types.Backend as B
import qualified Types.Remote as R
import qualified Remote
import qualified Backend

import System.Info

cmd :: Command
cmd = dontCheck repoExists $ noCommit $ 
	noRepo (seekNoRepo <$$> optParser) $ 
		command "version" SectionQuery "show version info"
			paramNothing (seek <$$> optParser)

data VersionOptions = VersionOptions
	{ rawOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser VersionOptions
optParser _ = VersionOptions
	<$> switch
		( long "raw"
		<> help "output only program version"
		)

seek :: VersionOptions -> CommandSeek
seek o
	| rawOption o = liftIO showRawVersion
	| otherwise = showVersion

seekNoRepo :: VersionOptions -> IO ()
seekNoRepo o
	| rawOption o = showRawVersion
	| otherwise = showPackageVersion

showVersion :: Annex ()
showVersion = do
	liftIO showPackageVersion
	maybe noop (liftIO . vinfo "local repository version" . showRepoVersion)
		=<< getVersion

showPackageVersion :: IO ()
showPackageVersion = do
	vinfo "git-annex version" BuildInfo.packageversion
        vinfo "Custom patch level" "2"
	vinfo "build flags" $ unwords buildFlags
	vinfo "dependency versions" $ unwords dependencyVersions
	vinfo "key/value backends" $ unwords $
		map (decodeBS . formatKeyVariety . B.backendVariety) Backend.builtinList
		++ ["X*"]
	vinfo "remote types" $ unwords $ map R.typename Remote.remoteTypes
	vinfo "operating system" $ unwords [os, arch]
	vinfo "supported repository versions" $
		verlist supportedVersions
	vinfo "upgrade supported from repository versions" $
		verlist upgradableVersions
  where
	verlist = unwords . map showRepoVersion

showRepoVersion :: RepoVersion -> String
showRepoVersion = show  . fromRepoVersion

showRawVersion :: IO ()
showRawVersion = do
	putStr BuildInfo.packageversion
	hFlush stdout -- no newline, so flush

vinfo :: String -> String -> IO ()
vinfo k v = putStrLn $ k ++ ": " ++ v
