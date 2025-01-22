{- Git smudge filter configuration
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Config.Smudge where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Command
import Git.Types
import Config
import Utility.Directory.Create
import Annex.Version
import qualified Utility.FileIO as F

import qualified System.FilePath.ByteString as P

configureSmudgeFilter :: Annex ()
configureSmudgeFilter = unlessM (fromRepo Git.repoIsLocalBare) $ do
	-- If this is run in a newly cloned repository, git may not have
	-- cached file information in the index yet, and so after
	-- configuring the clean filter, the next git status would want to
	-- run it on every file. That is expensive and can also result in
	-- unexpected changes when the file is checked into git or annex
	-- counter to the annex.largefiles configuration.
	-- Avoid that problem by running git status now.
	inRepo $ Git.Command.runQuiet
		[ Param "status"
		, Param "--porcelain"
		, Param "--ignore-submodules"
		]

	setConfig (ConfigKey "filter.annex.smudge") "git-annex smudge -- %f"
	setConfig (ConfigKey "filter.annex.clean") "git-annex smudge --clean -- %f"
	whenM (versionSupportsFilterProcess <$> getVersion)
		configureSmudgeFilterProcess
	lf <- Annex.fromRepo Git.attributesLocal
	gf <- Annex.fromRepo Git.attributes
	lfs <- readattr lf
	gfs <- readattr gf
	gittop <- Git.localGitDir <$> gitRepo
	liftIO $ unless ("filter=annex" `isInfixOf` (lfs ++ gfs)) $ do
		createDirectoryUnder [gittop] (P.takeDirectory lf)
		writeFile (fromRawFilePath lf) (lfs ++ "\n" ++ unlines stdattr)
  where
	readattr = liftIO . catchDefaultIO "" . readFileStrict . fromRawFilePath

configureSmudgeFilterProcess :: Annex ()
configureSmudgeFilterProcess =
	setConfig (ConfigKey "filter.annex.process") "git-annex filter-process"

stdattr :: [String]
stdattr =
	[ "* filter=annex"
	]

-- Note that this removes the local git attributes for filtering, 
-- which is what git-annex installed, but it does not change anything
-- that may have been committed to a .gitattributes in the repository.
-- git-annex does not commit that.
deconfigureSmudgeFilter :: Annex ()
deconfigureSmudgeFilter = do
	lf <- Annex.fromRepo Git.attributesLocal
	ls <- liftIO $ catchDefaultIO [] $ 
		map decodeBS . fileLines' <$> F.readFile' (toOsPath lf)
	liftIO $ writeFile (fromRawFilePath lf) $ unlines $
		filter (\l -> l `notElem` stdattr && not (null l)) ls
	unsetConfig (ConfigKey "filter.annex.smudge")
	unsetConfig (ConfigKey "filter.annex.clean")

-- Params to pass to git to temporarily avoid using the smudge/clean
-- filters.
bypassSmudgeConfig :: [CommandParam]
bypassSmudgeConfig = map Param
	[ "-c", "filter.annex.smudge="
	, "-c", "filter.annex.clean="
	, "-c", "filter.annex.process="
	]
