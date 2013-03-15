{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes, CPP #-}

module Assistant.WebApp.Configurators where

import Assistant.WebApp.Common
import Assistant.WebApp.RepoList
import Assistant.WebApp.Configurators.Local
#ifdef WITH_XMPP
import Assistant.XMPP.Client
#endif

{- The main configuration screen. -}
getConfigurationR :: Handler RepHtml
getConfigurationR = ifM (inFirstRun)
	( getFirstRepositoryR
	, page "Configuration" (Just Configuration) $ do
#ifdef WITH_XMPP
		xmppconfigured <- lift $ liftAnnex $ isJust <$> getXMPPCreds
#else
		let xmppconfigured = False
#endif
		$(widgetFile "configurators/main")
	)

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	repolist <- lift $ repoList $ RepoSelector
		{ onlyCloud = False
		, onlyConfigured = True
		, includeHere = False
		}
	let n = length repolist
	let numrepos = show n
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }

{- Lists known repositories, followed by options to add more. -}
getRepositoriesR :: Handler RepHtml
getRepositoriesR = page "Repositories" (Just Repositories) $ do
	let repolist = repoListDisplay $ RepoSelector
		{ onlyCloud = False
		, onlyConfigured = False
		, includeHere = True
		}
	$(widgetFile "configurators/repositories")


makeMiscRepositories :: Widget
makeMiscRepositories = $(widgetFile "configurators/repositories/misc")

makeCloudRepositories :: Bool -> Widget
makeCloudRepositories onlyTransfer = $(widgetFile "configurators/repositories/cloud")
