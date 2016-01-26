{- git-annex assistant alert utilities
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Alert.Utility where

import Annex.Common
import Assistant.Types.Alert
import Utility.Tense

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map as M

{- This is as many alerts as it makes sense to display at a time.
 - A display might be smaller, or larger, the point is to not overwhelm the
 - user with a ton of alerts. -}
displayAlerts :: Int
displayAlerts = 6

{- This is not a hard maximum, but there's no point in keeping a great
 - many filler alerts in an AlertMap, so when there's more than this many,
 - they start being pruned, down toward displayAlerts. -}
maxAlerts :: Int
maxAlerts = displayAlerts * 2

type AlertPair = (AlertId, Alert)

{- The desired order is the reverse of:
 -
 - - Pinned alerts
 - - High priority alerts, newest first
 - - Medium priority Activity, newest first (mostly used for Activity)
 - - Low priority alerts, newest first
 - - Filler priorty alerts, newest first
 - - Ties are broken by the AlertClass, with Errors etc coming first.
 -}
compareAlertPairs :: AlertPair -> AlertPair -> Ordering
compareAlertPairs
	(aid, Alert { alertClass = aclass, alertPriority = aprio })
	(bid, Alert { alertClass = bclass, alertPriority = bprio })
	 = compare aprio bprio
		`mappend` compare aid bid
			`mappend` compare aclass bclass

sortAlertPairs :: [AlertPair] -> [AlertPair]
sortAlertPairs = sortBy compareAlertPairs

{- Renders an alert's header for display, if it has one. -}
renderAlertHeader :: Alert -> Maybe Text
renderAlertHeader alert = renderTense (alertTense alert) <$> alertHeader alert

{- Renders an alert's message for display. -}
renderAlertMessage :: Alert -> Text
renderAlertMessage alert = renderTense (alertTense alert) $
	(alertMessageRender alert) alert

showAlert :: Alert -> String
showAlert alert = T.unpack $ T.unwords $ catMaybes
	[ renderAlertHeader alert
	, Just $ renderAlertMessage alert
	]

alertTense :: Alert -> Tense
alertTense alert
	| alertClass alert == Activity = Present
	| otherwise = Past

{- Checks if two alerts display the same. -}
effectivelySameAlert :: Alert -> Alert -> Bool
effectivelySameAlert x y = all id 
	[ alertClass x == alertClass y
	, alertHeader x == alertHeader y
	, alertData x == alertData y
	, alertBlockDisplay x == alertBlockDisplay y
	, alertClosable x == alertClosable y
	, alertPriority x == alertPriority y
	]

makeAlertFiller :: Bool -> Alert -> Alert
makeAlertFiller success alert
	| isFiller alert = alert
	| otherwise = alert
		{ alertClass = if c == Activity then c' else c
		, alertPriority = Filler
		, alertClosable = True
		, alertButtons = []
		, alertIcon = Just $ if success then SuccessIcon else ErrorIcon
		}
  where
	c = alertClass alert
	c'
		| success = Success
		| otherwise = Error

isFiller :: Alert -> Bool
isFiller alert = alertPriority alert == Filler

{- Updates the Alertmap, adding or updating an alert.
 -
 - Any old filler that looks the same as the alert is removed.
 -
 - Or, if the alert has an alertCombiner that combines it with
 - an old alert, the old alert is replaced with the result, and the
 - alert is removed.
 -
 - Old filler alerts are pruned once maxAlerts is reached.
 -}
mergeAlert :: AlertId -> Alert -> AlertMap -> AlertMap
mergeAlert i al m = maybe updatePrune updateCombine (alertCombiner al)
  where
	pruneSame k al' = k == i || not (effectivelySameAlert al al')
	pruneBloat m'
		| bloat > 0 = M.fromList $ pruneold $ M.toList m'
		| otherwise = m'
	  where
		bloat = M.size m' - maxAlerts
		pruneold l =
			let (f, rest) = partition (\(_, a) -> isFiller a) l
			in drop bloat f ++ rest
	updatePrune = pruneBloat $ M.filterWithKey pruneSame $
		M.insertWith' const i al m
	updateCombine combiner = 
		let combined = M.mapMaybe (combiner al) m
		in if M.null combined
			then updatePrune
			else M.delete i $ M.union combined m
