{- git-annex assistant unused file preferences
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Unused where

import Assistant.WebApp.Common
import qualified Annex
import Utility.HumanTime
import Assistant.Unused
import Config
import Git.Config
import Logs.Unused
import Utility.Tense

import qualified Text.Hamlet as Hamlet

data UnusedForm = UnusedForm
	{ enableExpire :: Bool
	, expireWhen :: Integer
	}

unusedForm :: UnusedForm -> Hamlet.Html -> MkMForm UnusedForm
unusedForm def msg = do
	(enableRes, enableView) <- mreq (selectFieldList enabledisable) ""
		(Just $ enableExpire def)
	(whenRes, whenView) <- mreq intField ""
		(Just $ expireWhen def)
	let form = do
		webAppFormAuthToken
		$(widgetFile "configurators/unused/form")
	return (UnusedForm <$> enableRes <*> whenRes, form)
  where
	enabledisable :: [(Text, Bool)]
	enabledisable = [("Disable expiry", False), ("Enable expiry", True)]

getConfigUnusedR :: Handler Html
getConfigUnusedR = postConfigUnusedR
postConfigUnusedR :: Handler Html
postConfigUnusedR = page "Unused files" (Just Configuration) $ do
	current <- liftAnnex getUnused
	((res, form), enctype) <- liftH $ runFormPostNoToken $ unusedForm current
	case res of
		FormSuccess new -> liftH $ do
			liftAnnex $ storeUnused new
			redirect ConfigurationR
		_ -> do
			munuseddesc <- liftAssistant describeUnused
			ts <- liftAnnex $ dateUnusedLog ""
			mlastchecked <- case ts of
				Nothing -> pure Nothing
				Just t -> Just <$> liftIO (durationSince t)
			$(widgetFile "configurators/unused")

getUnused :: Annex UnusedForm
getUnused = convert . annexExpireUnused <$> Annex.getGitConfig
  where
	convert Nothing = noexpire
	convert (Just Nothing) = noexpire
	convert (Just (Just n)) = UnusedForm True $ durationToDays n

	-- The 7 is so that, if they enable expiry, they have to change
	-- it to get faster than  a week.
	noexpire = UnusedForm False 7

storeUnused :: UnusedForm -> Annex ()
storeUnused f = setConfig (annexConfig "expireunused") $
	if not (enableExpire f) || expireWhen f < 0
		then boolConfig False
		else fromDuration $ daysToDuration $ expireWhen f

getCleanupUnusedR :: Handler Html
getCleanupUnusedR = do
	liftAssistant $ expireUnused Nothing
	redirect ConfigUnusedR
