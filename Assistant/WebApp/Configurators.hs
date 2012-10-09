{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Logs.Remote
import Logs.Trust

import Yesod
import Data.Text (Text)
import qualified Data.Map as M

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
	repolist <- lift $ repoList False
	$(widgetFile "configurators/repositories")

data SetupRepo = EnableRepo (Route WebApp) | EditRepo (Route WebApp)

needsEnabled :: SetupRepo -> Bool
needsEnabled (EnableRepo _) = True
needsEnabled _ = False

setupRepoLink :: SetupRepo -> Route WebApp
setupRepoLink (EnableRepo r) = r
setupRepoLink (EditRepo r) = r

{- A numbered list of known repositories, including the current one. -}
repoList :: Bool -> Handler [(String, String, SetupRepo)]
repoList onlyconfigured
	| onlyconfigured = list =<< configured
	| otherwise = list =<< (++) <$> configured <*> unconfigured
	where
		configured = do
			rs <- filter (not . Remote.readonly) . knownRemotes <$>
				(liftIO . getDaemonStatus =<< daemonStatus <$> getYesod)
			runAnnex [] $ do
				u <- getUUID
				let l = u : map Remote.uuid rs
				return $ zip l (map editlink l)
		editlink = EditRepo . EditRepositoryR
		unconfigured = runAnnex [] $ do
			m <- readRemoteLog
			catMaybes . map (findtype m) . snd
				<$> (trustPartition DeadTrusted $ M.keys m)
		findtype m u = case M.lookup u m of
			Nothing -> Nothing
			Just c -> case M.lookup "type" c of
				Just "rsync" -> u `enableswith` EnableRsyncR
				Just "directory" -> u `enableswith` EnableDirectoryR
				Just "S3" -> u `enableswith` EnableS3R
				_ -> Nothing
		u `enableswith` r = Just (u, EnableRepo $ r u)
		list l = runAnnex [] $ do
			let l' = nubBy (\x y -> fst x == fst y) l
			zip3
				<$> pure counter
				<*> Remote.prettyListUUIDs (map fst l')
				<*> pure (map snd l')
		counter = map show ([1..] :: [Int])

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift $ repoList True
	let n = length repolist
	let numrepos = show n
	let notenough = n < enough
	let barelyenough = n == enough
	let morethanenough = n > enough
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }
	where
		enough = 2
