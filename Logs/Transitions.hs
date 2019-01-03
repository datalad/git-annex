{- git-annex transitions log
 -
 - This is used to record transitions that have been performed on the
 - git-annex branch, and when the transition was first started.
 -
 - We can quickly detect when the local branch has already had an transition
 - done that is listed in the remote branch by checking that the local
 - branch contains the same transition, with the same or newer start time.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Transitions where

import Annex.Common
import Annex.VectorClock
import Logs.Line

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L

transitionsLog :: FilePath
transitionsLog = "transitions.log"

data Transition
	= ForgetGitHistory
	| ForgetDeadRemotes
	deriving (Show, Ord, Eq, Read)

data TransitionLine = TransitionLine
	{ transitionStarted :: VectorClock
	, transition :: Transition
	} deriving (Ord, Eq)

type Transitions = S.Set TransitionLine

describeTransition :: Transition -> String
describeTransition ForgetGitHistory = "forget git history"
describeTransition ForgetDeadRemotes = "forget dead remotes"

noTransitions :: Transitions
noTransitions = S.empty

addTransition :: VectorClock -> Transition -> Transitions -> Transitions
addTransition c t = S.insert $ TransitionLine c t

showTransitions :: Transitions -> String
showTransitions = unlines . map showTransitionLine . S.elems

{- If the log contains new transitions we don't support, returns Nothing. -}
parseTransitions :: String -> Maybe Transitions
parseTransitions = check . map parseTransitionLine . splitLines
  where
	check l
		| all isJust l = Just $ S.fromList $ catMaybes l
		| otherwise = Nothing

parseTransitionsStrictly :: String -> String -> Transitions
parseTransitionsStrictly source = fromMaybe badsource . parseTransitions
  where
	badsource = giveup $ "unknown transitions listed in " ++ source ++ "; upgrade git-annex!"

showTransitionLine :: TransitionLine -> String
showTransitionLine (TransitionLine c t) = unwords [show t, formatVectorClock c]

parseTransitionLine :: String -> Maybe TransitionLine
parseTransitionLine s = TransitionLine
	<$> parseVectorClock cs
	<*> readish ts
  where
	ws = words s
	ts = Prelude.head ws
	cs = unwords $ Prelude.tail ws

combineTransitions :: [Transitions] -> Transitions
combineTransitions = S.unions

transitionList :: Transitions -> [Transition]
transitionList = nub . map transition . S.elems

{- Typically ran with Annex.Branch.change, but we can't import Annex.Branch
 - here since it depends on this module. -}
recordTransitions :: (FilePath -> (L.ByteString -> L.ByteString) -> Annex ()) -> Transitions -> Annex ()
recordTransitions changer t = changer transitionsLog $
	encodeBL . showTransitions . S.union t . parseTransitionsStrictly "local" . decodeBL
