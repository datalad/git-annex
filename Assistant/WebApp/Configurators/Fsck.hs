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
import qualified Data.Text as T
import qualified Text.Hamlet as Hamlet

import Assistant.WebApp.Common
import Types.ScheduledActivity
import Utility.HumanTime
import Utility.Scheduled

fsckForm :: ScheduledActivity -> Hamlet.Html -> MkMForm ScheduledActivity
fsckForm (ScheduledSelfFsck (Schedule r t) d) msg = do
	(durationRes, durationView) <- mreq intField "" (Just $ durationSeconds d `quot` 60 )
	(timeRes, timeView) <- mreq (selectFieldList times) "" (Just t)
	(recurranceRes, recurranceView) <- mreq (selectFieldList recurrances) "" (Just r)
	let form = do
		webAppFormAuthToken
		$(widgetFile "configurators/fsck/form")
	let formresult = ScheduledSelfFsck
		<$> (Schedule <$> recurranceRes <*> timeRes)
		<*> (Duration <$> durationRes)
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
fsckForm (ScheduledRemoteFsck u s d) _msg = error "TODO"

defaultFsck :: ScheduledActivity
defaultFsck = ScheduledSelfFsck (Schedule Daily AnyTime) (Duration $ 60*60)

getConfigFsckR :: Handler Html
getConfigFsckR = postConfigFsckR
postConfigFsckR :: Handler Html
postConfigFsckR = page "Consistency checks" (Just Configuration) $ do
	((res, form), enctype) <- liftH $ runFormPost $ fsckForm defaultFsck
	case res of
		FormSuccess s -> error "TODO"
		_ -> $(widgetFile "configurators/fsck")
