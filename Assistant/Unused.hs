{- git-annex assistant unused files
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Assistant.Unused where

import qualified Data.Map as M

import Assistant.Common
import qualified Git
import Types.Key
import Logs.Unused
import Logs.Location
import Annex.Content
import Utility.DataUnits
import Utility.DiskFree
import Utility.HumanTime
import Utility.Tense

import Data.Time.Clock.POSIX
import qualified Data.Text as T

describeUnused :: Assistant (Maybe TenseText)
describeUnused = describeUnused' False

describeUnusedWhenBig :: Assistant (Maybe TenseText)
describeUnusedWhenBig = describeUnused' True

{- This uses heuristics: 1000 unused keys, or more unused keys 
 - than the remaining free disk space, or more than 1/10th the total
 - disk space being unused keys all suggest a problem. -}
describeUnused' :: Bool -> Assistant (Maybe TenseText)
describeUnused' whenbig = liftAnnex $ go =<< readUnusedLog ""
  where
	go m = do
		let num = M.size m
		let diskused = foldl' sumkeysize 0 (M.keys m)
		df <- forpath getDiskFree
		disksize <- forpath getDiskSize
		return $ if num == 0
			then Nothing
			else if not whenbig || moreused df diskused || tenthused disksize diskused
				then Just $ tenseWords
					[ UnTensed $ T.pack $ roughSize storageUnits False diskused
					, Tensed "are" "were"
					, "taken up by unused files"
					]
				else if num > 1000
					then Just $ tenseWords
						[ UnTensed $ T.pack $ show num ++ " unused files"
						, Tensed "exist" "existed"
						]
					else Nothing

	moreused Nothing _ = False
	moreused (Just df) used = df <= used

	tenthused Nothing _ = False
	tenthused (Just disksize) used = used >= disksize `div` 10

	sumkeysize s k = s + fromMaybe 0 (keySize k)

	forpath a = inRepo $ liftIO . a . Git.repoPath

{- With a duration, expires all unused files that are older.
 - With Nothing, expires *all* unused files. -}
expireUnused :: Maybe Duration -> Assistant ()
expireUnused duration = do
	m <- liftAnnex $ readUnusedLog ""
	now <- liftIO getPOSIXTime
	let oldkeys = M.keys $ M.filter (tooold now) m
	forM_ oldkeys $ \k -> do
		debug ["removing old unused key", key2file k]
		liftAnnex $ do
			lockContentForRemoval k removeAnnex
			logStatus k InfoMissing
  where
	boundry = durationToPOSIXTime <$> duration
	tooold now (_, mt) = case boundry of
		Nothing -> True
		Just b -> maybe False (\t -> now - t >= b) mt
