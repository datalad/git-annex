{- git-annex safe drop proof
 -
 - Copyright 2014-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.SafeDropProof (
	SafeDropProof,
	safeDropProofEndTime,
	safeDropProofExpired,
	checkSafeDropProofEndTime,
) where

import Annex.Common
import Types.NumCopies

import Data.Time.Clock.POSIX

safeDropProofExpired :: Annex ()
safeDropProofExpired = do
	showNote "unsafe"
	showLongNote $ UnquotedString
		"Dropping took too long, and locks may have expired."

checkSafeDropProofEndTime :: Maybe SafeDropProof -> IO Bool
checkSafeDropProofEndTime p = case safeDropProofEndTime =<< p of
	Nothing -> return True
	Just endtime -> do
		now <- getPOSIXTime
		return (endtime > now)

