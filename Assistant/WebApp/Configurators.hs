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
import Assistant.WebApp.SideBar
import Utility.Yesod
import qualified Remote
import Logs.Web (webUUID)
import Logs.Trust
import Annex.UUID (getUUID)

import Yesod
import Data.Text (Text)

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	l <- lift $ runAnnex [] $ do
		u <- getUUID
		rs <- map Remote.uuid <$> Remote.remoteList
		rs' <- snd <$> trustPartition DeadTrusted rs
		Remote.prettyListUUIDs $ filter (/= webUUID) $ nub $ u:rs'
	let remotelist = zip counter l
	let n = length l
	let numrepos = show n
	let notenough = n < 2
	let barelyenough = n == 2
	let morethanenough = n > 2
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }
	where
		counter = map show ([1..] :: [Int])

getConfigR :: Handler RepHtml
getConfigR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Configuration"
	$(widgetFile "configurators/main")

getAddRepositoryR :: Handler RepHtml
getAddRepositoryR = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Add repository"
	$(widgetFile "configurators/addrepository")
