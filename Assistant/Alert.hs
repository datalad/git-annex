{- git-annex assistant alerts
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Assistant.Alert where

import Yesod

type Widget = forall sub master. GWidget sub master ()

{- Different classes of alerts are displayed differently. -}
data AlertClass = Activity | Warning | Error | Success | Message
	deriving (Eq)

{- An alert can be a simple message, or an arbitrary Yesod Widget -}
data AlertMessage = StringAlert String | WidgetAlert Widget

data Alert = Alert
	{ alertClass :: AlertClass
	, alertHeader :: Maybe String
	, alertMessage :: AlertMessage
	, alertBlockDisplay :: Bool
	}

activityAlert :: Maybe String -> String -> Alert
activityAlert header message = Alert
	{ alertClass = Activity
	, alertHeader = header
	, alertMessage = StringAlert message
	, alertBlockDisplay = False
	}
