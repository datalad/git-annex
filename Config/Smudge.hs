{- Git smudge filter configuration
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
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

configureSmudgeFilter :: Annex ()
configureSmudgeFilter = unlessM (fromRepo Git.repoIsLocalBare) $ do
	-- If this is run in a newly cloned repository, git may not have
	-- cached file information in the index yet, and so after
	-- configuring the clean filter, the next git status would want to
	-- run it on every file. That is expensive and can also result in
	-- unexpected changes when the file is checked into git or annex
	-- counter to the annex.largefiles configuration.
	-- Avoid that problem by running git status now.
	inRepo $ Git.Command.runQuiet [Param "status", Param "--porcelain"]

	setConfig (ConfigKey "filter.annex.smudge") "git-annex smudge -- %f"
	setConfig (ConfigKey "filter.annex.clean") "git-annex smudge --clean -- %f"
	lf <- Annex.fromRepo Git.attributesLocal
	gf <- Annex.fromRepo Git.attributes
	lfs <- readattr lf
	gfs <- readattr gf
	liftIO $ unless ("filter=annex" `isInfixOf` (lfs ++ gfs)) $ do
		createDirectoryIfMissing True (takeDirectory lf)
		writeFile lf (lfs ++ "\n" ++ unlines stdattr)
  where
	readattr = liftIO . catchDefaultIO "" . readFileStrict

stdattr :: [String]
stdattr =
	[ "* filter=annex"
	, ".* !filter"
	]

-- Note that this removes the local git attributes for filtering, 
-- which is what git-annex installed, but it does not change anything
-- that may have been committed to a .gitattributes in the repository.
-- git-annex does not commit that.
deconfigureSmudgeFilter :: Annex ()
deconfigureSmudgeFilter = do
	lf <- Annex.fromRepo Git.attributesLocal
	ls <- liftIO $ catchDefaultIO [] $ lines <$> readFileStrict lf
	liftIO $ writeFile lf $ unlines $
		filter (\l -> l `notElem` stdattr && not (null l)) ls
	unsetConfig (ConfigKey "filter.annex.smudge")
	unsetConfig (ConfigKey "filter.annex.clean")
