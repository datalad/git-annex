{- git-annex assistant general preferences
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Preferences (
	getPreferencesR,
	postPreferencesR
) where

import Assistant.WebApp.Common
import qualified Annex
import qualified Git
import Config
import Config.Files
import Annex.NumCopies
import Utility.DataUnits
import Git.Config
import Types.Distribution
import qualified Build.SysConfig

import qualified Data.Text as T

data PrefsForm = PrefsForm
	{ diskReserve :: Text
	, numCopies :: Int
	, autoStart :: Bool
	, autoUpgrade :: AutoUpgrade
	, enableDebug :: Bool
	}

prefsAForm :: PrefsForm -> MkAForm PrefsForm
prefsAForm d = PrefsForm
	<$> areq (storageField `withNote` diskreservenote)
		(bfs "Disk reserve") (Just $ diskReserve d)
	<*> areq (positiveIntField `withNote` numcopiesnote)
		(bfs "Number of copies") (Just $ numCopies d)
	<*> areq (checkBoxField `withNote` autostartnote)
		"Auto start" (Just $ autoStart d)
	<*> areq (selectFieldList autoUpgradeChoices)
		(bfs autoUpgradeLabel) (Just $ autoUpgrade d)
	<*> areq (checkBoxField `withNote` debugnote)
		"Enable debug logging" (Just $ enableDebug d)
  where
	diskreservenote = [whamlet|<br>Avoid downloading files from other repositories when there is too little free disk space.|]
	numcopiesnote = [whamlet|<br>Only drop a file after verifying that other repositories contain this many copies.|]
	debugnote = [whamlet|<a href="@{LogR}">View Log</a>|]
	autostartnote = [whamlet|Start the git-annex assistant at boot or on login.|]

	autoUpgradeChoices :: [(Text, AutoUpgrade)]
	autoUpgradeChoices =
		[ ("ask me", AskUpgrade)
		, ("enabled", AutoUpgrade)
		, ("disabled", NoAutoUpgrade)
		]
	autoUpgradeLabel
		| isJust Build.SysConfig.upgradelocation = "Auto upgrade"
		| otherwise = "Auto restart on upgrade"

	positiveIntField = check isPositive intField
	  where
		isPositive i
			| i > 0 = Right i
			| otherwise = Left notPositive
		notPositive :: Text
		notPositive = "This should be 1 or more!"

	storageField = check validStorage textField
	  where
		validStorage t
			| T.null t = Right t
			| otherwise =  case readSize dataUnits $ T.unpack t of
				Nothing -> Left badParse
				Just _ -> Right t
		badParse :: Text
		badParse = "Parse error. Expected something like \"100 megabytes\" or \"2 gb\""

getPrefs :: Annex PrefsForm
getPrefs = PrefsForm
	<$> (T.pack . roughSize storageUnits False . annexDiskReserve <$> Annex.getGitConfig)
	<*> (fromNumCopies <$> getNumCopies)
	<*> inAutoStartFile
	<*> (annexAutoUpgrade <$> Annex.getGitConfig)
	<*> (annexDebug <$> Annex.getGitConfig)

storePrefs :: PrefsForm -> Annex ()
storePrefs p = do
	setConfig (annexConfig "diskreserve") (T.unpack $ diskReserve p)
	setGlobalNumCopies (NumCopies $ numCopies p)
	unsetConfig (annexConfig "numcopies") -- deprecated
	setConfig (annexConfig "autoupgrade") (fromAutoUpgrade $ autoUpgrade p)
	unlessM ((==) <$> pure (autoStart p) <*> inAutoStartFile) $ do
		here <- fromRepo Git.repoPath
		liftIO $ if autoStart p
			then addAutoStartFile here
			else removeAutoStartFile here
	setConfig (annexConfig "debug") (boolConfig $ enableDebug p)
	liftIO $ if enableDebug p
		then enableDebugOutput 
		else disableDebugOutput

getPreferencesR :: Handler Html
getPreferencesR = postPreferencesR
postPreferencesR :: Handler Html
postPreferencesR = page "Preferences" (Just Configuration) $ do
	((result, form), enctype) <- liftH $ do
		current <- liftAnnex getPrefs
		runFormPostNoToken $ renderBootstrap3 bootstrapFormLayout $ prefsAForm current
	case result of
		FormSuccess new -> liftH $ do
			liftAnnex $ storePrefs new
			redirect ConfigurationR
		_ -> $(widgetFile "configurators/preferences")

inAutoStartFile :: Annex Bool
inAutoStartFile = do
	here <- fromRepo Git.repoPath
	any (`equalFilePath` here) <$> liftIO readAutoStartFile
