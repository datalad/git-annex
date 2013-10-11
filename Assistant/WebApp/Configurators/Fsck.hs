{- git-annex assistant fsck configuration
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Assistant.WebApp.Configurators.Fsck where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Hamlet as Hamlet

import Assistant.WebApp.Common
import Types.ScheduledActivity
import Utility.HumanTime
import Utility.Scheduled
import Logs.Schedule
import Annex.UUID

{- This adds a form to the page. It does not handle posting of the form,
 - because unlike a typical yesod form that posts using the same url
 - that generated it, this form posts using one of two other routes. -}
fsckForm :: Bool -> ScheduledActivity -> Widget
fsckForm new activity = do
	u <- liftAnnex getUUID
	let action = if new
		then AddActivityR u
		else ChangeActivityR u activity
	((res, form), enctype) <- liftH $ runFormPost $ fsckForm' new activity
	case res of
		FormSuccess _ -> noop
		_ -> $(widgetFile "configurators/fsck/form")

{- This does not display a form, but it does get it from a post, and run
 - some Annex action on it. -}
withFsckForm :: (ScheduledActivity -> Annex ()) -> Handler ()
withFsckForm a = do
	((res, _form), _enctype) <- runFormPost $ fsckForm' False defaultFsck
	case res of
		FormSuccess activity -> liftAnnex $ a activity
		_ -> noop

fsckForm' :: Bool -> ScheduledActivity -> Hamlet.Html -> MkMForm ScheduledActivity
fsckForm' new activity@(ScheduledSelfFsck (Schedule r t) d) msg = do
	(durationRes, durationView) <- mreq intField "" (Just $ durationSeconds d `quot` 60 )
	(timeRes, timeView) <- mreq (selectFieldList times) "" (Just t)
	(recurranceRes, recurranceView) <- mreq (selectFieldList recurrances) "" (Just r)
	let form = do
		webAppFormAuthToken
		u <- liftAnnex getUUID
		$(widgetFile "configurators/fsck/formcontent")
	let formresult = ScheduledSelfFsck
		<$> (Schedule <$> recurranceRes <*> timeRes)
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
		, ("every Sunday", Weekly 1)
		, ("every Monday", Weekly 2)
		, ("every Tuesday", Weekly 3)
		, ("every Wednesday", Weekly 4)
		, ("every Thursday", Weekly 5)
		, ("every Friday", Weekly 6)
		, ("every Saturday", Weekly 7)
		, ("on the 1st of the month", Monthly 1)
		, ("mid-month", Monthly 15)
		, ("once a year", Yearly 1)
		, ("twice a year", (Divisible 6 $ Monthly 1))
		, ("quarterly", (Divisible 4 $ Monthly 1))
		]
	ensurevalue v desc l = case M.lookup v (M.fromList $ map (\(x,y) -> (y,x)) l) of
		Just _ -> l
		Nothing -> (desc, v) : l
fsckForm' new (ScheduledRemoteFsck u s d) _msg = error "TODO"

defaultFsck :: ScheduledActivity
defaultFsck = ScheduledSelfFsck (Schedule Daily AnyTime) (Duration $ 60*60)

getConfigFsckR :: Handler Html
getConfigFsckR = postConfigFsckR
postConfigFsckR :: Handler Html
postConfigFsckR = page "Consistency checks" (Just Configuration) $ do
	checks <- liftAnnex $ S.toList <$> (scheduleGet =<< getUUID)
	$(widgetFile "configurators/fsck")

getRemoveActivityR :: UUID -> ScheduledActivity -> Handler Html
getRemoveActivityR u activity = do
	liftAnnex $ scheduleRemove u activity
	redirect ConfigFsckR

getAddActivityR :: UUID -> Handler Html
getAddActivityR = postAddActivityR
postAddActivityR :: UUID -> Handler Html
postAddActivityR u = do
	withFsckForm $ scheduleAdd u
	redirect ConfigFsckR

getChangeActivityR :: UUID -> ScheduledActivity -> Handler Html
getChangeActivityR = postChangeActivityR
postChangeActivityR :: UUID -> ScheduledActivity -> Handler Html
postChangeActivityR u oldactivity = do
	withFsckForm $ \newactivity -> scheduleChange u $
			S.insert newactivity . S.delete oldactivity
	redirect ConfigFsckR
