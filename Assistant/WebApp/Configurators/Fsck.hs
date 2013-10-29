{- git-annex assistant fsck configuration
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Assistant.WebApp.Configurators.Fsck where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

import Assistant.WebApp.Common
import Types.ScheduledActivity
import Utility.HumanTime
import Utility.Scheduled
import Logs.Schedule
import Annex.UUID
import qualified Remote
import Assistant.DaemonStatus
import qualified Annex.Branch
import Assistant.Fsck
import Config
import Git.Config
import qualified Annex

{- This adds a form to the page. It does not handle posting of the form,
 - because unlike a typical yesod form that posts using the same url
 - that generated it, this form posts using one of two other routes. -}
showFsckForm :: Bool -> ScheduledActivity -> Widget
showFsckForm new activity = do
	u <- liftAnnex getUUID
	let action = if new
		then AddActivityR u
		else ChangeActivityR u activity
	((res, form), enctype) <- liftH $ runFsckForm new activity
	case res of
		FormSuccess _ -> noop
		_ -> $(widgetFile "configurators/fsck/form")

{- This does not display a form, but it does get it from a post, and run
 - some Annex action on it. -}
withFsckForm :: (ScheduledActivity -> Annex ()) -> Handler ()
withFsckForm a = do
	((res, _form), _enctype) <- runFsckForm False $ defaultFsck Nothing
	case res of
		FormSuccess activity -> liftAnnex $ a activity
		_ -> noop

mkFsck :: UUID -> UUID -> Schedule -> Duration -> ScheduledActivity
mkFsck hereu u s d
	| u == hereu = ScheduledSelfFsck s d 
	| otherwise = ScheduledRemoteFsck u s d

runFsckForm :: Bool -> ScheduledActivity -> Handler ((FormResult ScheduledActivity, Widget), Enctype)
runFsckForm new activity = case activity of
	ScheduledSelfFsck s d -> go s d =<< liftAnnex getUUID
	ScheduledRemoteFsck ru s d -> go s d ru
  where
  	go (Schedule r t) d ru = do
		u <- liftAnnex getUUID
		repolist <- liftAssistant (getrepolist ru)
		runFormPostNoToken $ \msg -> do
			(reposRes, reposView) <- mreq (selectFieldList repolist) "" (Just ru)
			(durationRes, durationView) <- mreq intField "" (Just $ durationSeconds d `quot` 60 )
			(timeRes, timeView) <- mreq (selectFieldList times) "" (Just t)
			(recurranceRes, recurranceView) <- mreq (selectFieldList recurrances) "" (Just r)
			let form = do
				webAppFormAuthToken
				$(widgetFile "configurators/fsck/formcontent")
			let formresult = mkFsck
				<$> pure u
				<*> reposRes
				<*> (Schedule <$> recurranceRes <*> timeRes)
				<*> (Duration <$> ((60 *) <$> durationRes))
			return (formresult, form)
	  where
		times :: [(Text, ScheduledTime)]
		times = ensurevalue t (T.pack $ fromScheduledTime t) $
			map (\x -> (T.pack $ fromScheduledTime x, x)) $
				AnyTime : map (\h -> SpecificTime h 0) [0..23]
		recurrances :: [(Text, Recurrance)]
		recurrances = ensurevalue r (T.pack $ fromRecurrance r) $
			[ ("every day", Daily)
			, ("every Sunday", Weekly $ Just 1)
			, ("every Monday", Weekly $ Just 2)
			, ("every Tuesday", Weekly $ Just 3)
			, ("every Wednesday", Weekly $ Just 4)
			, ("every Thursday", Weekly $ Just 5)
			, ("every Friday", Weekly $ Just 6)
			, ("every Saturday", Weekly $ Just 7)
			, ("monthly", Monthly Nothing)
			, ("twice a month", Divisible 2 (Weekly Nothing))
			, ("yearly", Yearly Nothing)
			, ("twice a year", Divisible 6 (Monthly Nothing))
			, ("quarterly", Divisible 4 (Monthly Nothing))
			]
	ensurevalue v desc l = case M.lookup v (M.fromList $ map (\(x,y) -> (y,x)) l) of
		Just _ -> l
		Nothing -> (desc, v) : l
	getrepolist :: UUID -> Assistant [(Text, UUID)]
	getrepolist ensureu = do
		-- It is possible to have fsck jobs for remotes that
		-- do not implement remoteFsck, but it's not too useful,
		-- so omit them from the UI normally.
		remotes <- filter (\r -> Remote.uuid r == ensureu || isJust (Remote.remoteFsck r)) . syncRemotes
			<$> getDaemonStatus
		u <- liftAnnex getUUID
		let us = u : (map Remote.uuid remotes)
		liftAnnex $ 
			zip <$> (map T.pack <$> Remote.prettyListUUIDs us) <*> pure us

defaultFsck :: Maybe Remote -> ScheduledActivity
defaultFsck Nothing = ScheduledSelfFsck (Schedule Daily AnyTime) (Duration $ 60*60)
defaultFsck (Just r) = ScheduledRemoteFsck (Remote.uuid r) (Schedule Daily AnyTime) (Duration $ 60*60)

showFsckStatus :: ScheduledActivity -> Widget
showFsckStatus activity = do
	m <- liftAnnex getLastRunTimes
	let lastrun = M.lookup activity m
	$(widgetFile "configurators/fsck/status")

getConfigFsckR :: Handler Html
getConfigFsckR = postConfigFsckR
postConfigFsckR :: Handler Html
postConfigFsckR = page "Consistency checks" (Just Configuration) $ do
	scheduledchecks <- liftAnnex $
		S.toList <$> (scheduleGet =<< getUUID)
	rs <- liftAssistant $
		filter fsckableRemote . syncRemotes <$> getDaemonStatus
	recommendedchecks <- liftAnnex $ map defaultFsck
		<$> filterM (not <$$> checkFscked) (Nothing : map Just rs)
	$(widgetFile "configurators/fsck")

changeSchedule :: Handler () -> Handler Html
changeSchedule a = do
	a
	liftAnnex $ Annex.Branch.commit "update"
	redirect ConfigFsckR

getRemoveActivityR :: UUID -> ScheduledActivity -> Handler Html
getRemoveActivityR u activity = changeSchedule $
	liftAnnex $ scheduleRemove u activity

getAddActivityR :: UUID -> Handler Html
getAddActivityR = postAddActivityR
postAddActivityR :: UUID -> Handler Html
postAddActivityR u = changeSchedule $
	withFsckForm $ scheduleAdd u

getChangeActivityR :: UUID -> ScheduledActivity -> Handler Html
getChangeActivityR = postChangeActivityR
postChangeActivityR :: UUID -> ScheduledActivity -> Handler Html
postChangeActivityR u oldactivity = changeSchedule $
	withFsckForm $ \newactivity -> scheduleChange u $
			S.insert newactivity . S.delete oldactivity

data FsckPreferences = FsckPreferences
	{ enableFsckNudge :: Bool
	}

getFsckPreferences :: Annex FsckPreferences
getFsckPreferences = FsckPreferences
	<$> (annexFsckNudge <$> Annex.getGitConfig)

fsckPreferencesAForm :: FsckPreferences -> MkAForm FsckPreferences
fsckPreferencesAForm def = FsckPreferences
	<$> areq (checkBoxField `withNote` nudgenote) "Reminders" (Just $ enableFsckNudge def)
  where
	nudgenote = [whamlet|Remind me when using repositories that lack consistency checks.|]

runFsckPreferencesForm :: Handler ((FormResult FsckPreferences, Widget), Enctype)
runFsckPreferencesForm = do
	prefs <- liftAnnex getFsckPreferences
	runFormPostNoToken $ renderBootstrap $ fsckPreferencesAForm prefs

showFsckPreferencesForm :: Widget
showFsckPreferencesForm = do
	((res, form), enctype) <- liftH $ runFsckPreferencesForm
	case res of
		FormSuccess _ -> noop
		_ -> $(widgetFile "configurators/fsck/preferencesform")

postConfigFsckPreferencesR :: Handler Html
postConfigFsckPreferencesR = do
	((res, _form), _enctype) <- runFsckPreferencesForm
	case res of
		FormSuccess prefs ->
			liftAnnex $ setConfig (annexConfig "fscknudge")
				(boolConfig $ enableFsckNudge prefs)
		_ -> noop
	redirect ConfigFsckR
