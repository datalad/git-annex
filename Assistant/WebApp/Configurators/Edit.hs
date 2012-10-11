{- git-annex assistant webapp configurator for editing existing repos
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.Edit where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.WebApp.SideBar
import Assistant.WebApp.Configurators.Local (syncRemote)
import Assistant.WebApp.DashBoard (cancelTransfer)
import Assistant.DaemonStatus
import Assistant.TransferQueue
import Assistant.ThreadedMonad
import Utility.Yesod
import qualified Remote
import qualified Remote.List as Remote
import qualified Types.Remote as Remote
import Logs.UUID
import Logs.Group
import Logs.PreferredContent
import Logs.Transfer
import Types.StandardGroups
import Config
import Annex.UUID

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

data RepoGroup = RepoGroupCustom String | RepoGroupStandard StandardGroup
	deriving (Show, Eq)

data RepoConfig = RepoConfig
	{ repoDescription :: Text
	, repoGroup :: RepoGroup
	, repoEnabled :: Bool
	}
	deriving (Show)

getRepoConfig :: Remote -> Annex RepoConfig
getRepoConfig r = RepoConfig
	<$> (T.pack . fromMaybe "" . M.lookup uuid <$> uuidMap)
	<*> getrepogroup
	<*> (thisrepo <||> (elem r <$> Remote.enabledRemoteList))
	where
		getrepogroup = do
			groups <- lookupGroups uuid
			return $ 
				maybe (RepoGroupCustom $ unwords $ S.toList groups) RepoGroupStandard
					(getStandardGroup groups)
		thisrepo = (==) uuid <$> getUUID
		uuid = Remote.uuid r

{- Returns Just False if the repository has been disabled,
 - or Just True when enabled. -}
setRepoConfig :: Remote -> RepoConfig -> Annex (Maybe Bool)
setRepoConfig r c = do
	describeUUID uuid $ T.unpack $ repoDescription c
	case repoGroup c of
		RepoGroupStandard g -> setStandardGroup uuid g
		RepoGroupCustom s -> groupSet uuid $ S.fromList $ words s
	ifM ((==) uuid <$> getUUID)
		( return Nothing
		, do
			enabled <- elem r <$> Remote.enabledRemoteList
			if (enabled /= repoEnabled c)
				then do
					setConfig annex_ignore $
						if enabled then "true" else "false"
					void $ Remote.remoteListRefresh
					return $ Just $ repoEnabled c
				else return Nothing
		)
	where
		uuid = Remote.uuid r
		annex_ignore = remoteConfig (Remote.repo r) "ignore"

changeEnabled :: Remote -> Bool -> Handler ()
changeEnabled r True = syncRemote r
changeEnabled r False = do
	webapp <- getYesod
	let dstatus = daemonStatus webapp
	let st = fromJust $ threadState webapp
	liftIO $ runThreadState st $ updateKnownRemotes dstatus
	{- Stop all transfers to or from this disabled remote.
	 - XXX Can't stop any ongoing scan, or git syncs. -}
	void $ liftIO $ dequeueTransfers (transferQueue webapp) dstatus tofrom
	mapM_ (cancelTransfer False) =<<
		filter tofrom . M.keys <$>
			liftIO (currentTransfers <$> getDaemonStatus dstatus)
	where
		tofrom t = transferUUID t == Remote.uuid r

editRepositoryAForm :: RepoConfig -> AForm WebApp WebApp RepoConfig
editRepositoryAForm def = RepoConfig
	<$> areq textField "Description" (Just $ repoDescription def)
	<*> areq (selectFieldList $ customgroups++standardgroups) "Repository group" (Just $ repoGroup def)
	<*> areq checkBoxField "Syncing enabled" (Just $ repoEnabled def)
	where
		standardgroups :: [(Text, RepoGroup)]
		standardgroups = map (\g -> (T.pack $ descStandardGroup g , RepoGroupStandard g))
			[minBound :: StandardGroup .. maxBound :: StandardGroup]
		customgroups :: [(Text, RepoGroup)]
		customgroups = case repoGroup def of
			RepoGroupCustom s -> [(T.pack s, RepoGroupCustom s)]
			_ -> []

getEditRepositoryR :: UUID -> Handler RepHtml
getEditRepositoryR uuid = bootstrap (Just Config) $ do
	sideBarDisplay
	setTitle "Configure repository"

	r <- lift $ fromMaybe (error "Unknown UUID") . M.lookup uuid
		<$> runAnnex M.empty (Remote.remoteMap id)
	curr <- lift $ runAnnex undefined $ getRepoConfig r
	((result, form), enctype) <- lift $
		runFormGet $ renderBootstrap $ editRepositoryAForm curr
	case result of
		FormSuccess input -> lift $ do
			maybe noop (changeEnabled r) =<<
				runAnnex undefined (setRepoConfig r input)
			redirect RepositoriesR
		_ -> showform form enctype
	where
		showform form enctype = do
			let authtoken = webAppFormAuthToken
			description <- lift $
				runAnnex T.empty $  T.pack . concat <$>
					Remote.prettyListUUIDs [uuid]
			$(widgetFile "configurators/editrepository")
