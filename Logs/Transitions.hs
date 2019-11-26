{- git-annex transitions log
 -
 - This is used to record transitions that have been performed on the
 - git-annex branch, and when the transition was first started.
 -
 - We can quickly detect when the local branch has already had an transition
 - done that is listed in the remote branch by checking that the local
 - branch contains the same transition, with the same or newer start time.
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Transitions where

import Annex.Common
import Annex.VectorClock
import Logs.Line

import qualified Data.Set as S
import Data.Either
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8

transitionsLog :: RawFilePath
transitionsLog = "transitions.log"

data Transition
	= ForgetGitHistory
	| ForgetDeadRemotes
	deriving (Show, Ord, Eq, Read)

data TransitionLine = TransitionLine
	{ transitionStarted :: VectorClock
	-- New transitions that we don't know about yet are preserved.
	, transition :: Either ByteString Transition
	} deriving (Ord, Eq)

type Transitions = S.Set TransitionLine

describeTransition :: Transition -> String
describeTransition ForgetGitHistory = "forget git history"
describeTransition ForgetDeadRemotes = "forget dead remotes"

noTransitions :: Transitions
noTransitions = S.empty

addTransition :: VectorClock -> Transition -> Transitions -> Transitions
addTransition c t = S.insert $ TransitionLine c (Right t)

buildTransitions :: Transitions -> Builder
buildTransitions = mconcat . map genline . S.elems
  where
	genline tl = buildt (transition tl) <> charUtf8 ' '
		<> buildVectorClock (transitionStarted tl) <> charUtf8 '\n'
	buildt (Left b) = byteString b
	buildt (Right t) = byteString (encodeBS (show t))

parseTransitions :: L.ByteString -> Transitions
parseTransitions = fromMaybe S.empty . A.maybeResult . A.parse
	(S.fromList <$> parseLogLines transitionLineParser)

parseTransitionsStrictly :: String -> L.ByteString -> Transitions
parseTransitionsStrictly source b =
	let ts = parseTransitions b
	in if S.null $ S.filter (isLeft . transition) ts
		then ts
		else giveup $ "unknown transitions listed in " ++ source ++ "; upgrade git-annex!"

showTransitionLine :: TransitionLine -> String
showTransitionLine (TransitionLine c t) = unwords [show t, formatVectorClock c]

transitionLineParser :: A.Parser TransitionLine
transitionLineParser = do
	t <- (parsetransition <$> A.takeByteString)
	_ <- A8.char ' '
	c <- vectorClockParser
	return $ TransitionLine c t
  where
	parsetransition b = case readish (decodeBS b) of
		Just t -> Right t
		Nothing -> Left b

combineTransitions :: [Transitions] -> Transitions
combineTransitions = S.unions

{- Unknown types of transitions are omitted. -}
knownTransitionList :: Transitions -> [Transition]
knownTransitionList = nub . rights . map transition . S.elems

{- Typically ran with Annex.Branch.change, but we can't import Annex.Branch
 - here since it depends on this module. -}
recordTransitions :: (RawFilePath -> (L.ByteString -> Builder) -> Annex ()) -> Transitions -> Annex ()
recordTransitions changer t = changer transitionsLog $
	buildTransitions . S.union t . parseTransitionsStrictly "local"
