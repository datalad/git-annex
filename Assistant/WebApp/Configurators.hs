{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.DaemonStatus
import Assistant.WebApp.Configurators.Local
import Utility.Yesod
import qualified Remote
import qualified Types.Remote as Remote
import Annex.UUID (getUUID)

import Yesod
import Data.Text (Text)

{- The main configuration screen. -}
getConfigR :: Handler RepHtml
getConfigR = ifM (inFirstRun)
	( getFirstRepositoryR
	, bootstrap (Just Config) $ do
		sideBarDisplay
		setTitle "Configuration"
		$(widgetFile "configurators/main")
	)

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Repositories"
	repolist <- lift repoList
	$(widgetFile "configurators/repositories")

{- A numbered list of known repositories, including the current one. -}
repoList :: Handler [(String, String)]
repoList = do
	rs <- filter (not . Remote.readonly) . knownRemotes <$>
		(liftIO . getDaemonStatus =<< daemonStatus <$> getYesod)
	l <- runAnnex [] $ do
		u <- getUUID
		Remote.prettyListUUIDs $ nub $ u:(map Remote.uuid rs)
	return $ zip counter l
	where
		counter = map show ([1..] :: [Int])

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift repoList
	let n = length repolist
	let numrepos = show n
	let notenough = n < enough
	let barelyenough = n == enough
	let morethanenough = n > enough
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }
	where
		enough = 2
