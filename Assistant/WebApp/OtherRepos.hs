{- git-annex assistant webapp switching to other repos
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.OtherRepos where

import Assistant.Common
import Assistant.WebApp.Types
import Assistant.WebApp.Page
import Config.Files.AutoStart
import Utility.Yesod
import Assistant.Restart
import qualified Utility.RawFilePath as R

getRepositorySwitcherR :: Handler Html
getRepositorySwitcherR = page "Switch repository" Nothing $ do
	repolist <- liftIO listOtherRepos
	$(widgetFile "control/repositoryswitcher")

listOtherRepos :: IO [(String, String)]
listOtherRepos = do
	dirs <- readAutoStartFile
	pwd <- R.getCurrentDirectory
	gooddirs <- filterM isrepo $
		filter (\d -> not $ toRawFilePath d `dirContains` pwd) dirs
	names <- mapM relHome gooddirs
	return $ sort $ zip names gooddirs
  where
	isrepo d = doesDirectoryExist (d </> ".git")

getSwitchToRepositoryR :: FilePath -> Handler Html
getSwitchToRepositoryR repo = do
	liftIO $ addAutoStartFile repo -- make this the new default repo
	redirect =<< liftIO (newAssistantUrl repo)
