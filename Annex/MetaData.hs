{- git-annex metadata
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.MetaData where

import Common.Annex
import qualified Annex
import Types.MetaData
import Logs.MetaData

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX

tagMetaField :: MetaField
tagMetaField = MetaField "tag"

yearMetaField :: MetaField
yearMetaField = MetaField "year"

monthMetaField :: MetaField
monthMetaField = MetaField "month"

{- Generates metadata for a file that has just been ingested into the
 - annex. Passed the FileStatus of the content file.
 -
 - Does not overwrite any existing metadata values for the key.
 -}
genMetaData :: Key -> FileStatus -> Annex ()
genMetaData key status = whenM (annexGenMetaData <$> Annex.getGitConfig) $ do
	metadata <- getCurrentMetaData key
	let metadata' = genMetaData' status metadata
	unless (metadata' == emptyMetaData) $
		addMetaData key metadata'

genMetaData' :: FileStatus -> MetaData -> MetaData
genMetaData' status old = MetaData $ M.fromList $ filter isnew
	[ (yearMetaField, S.singleton $ toMetaValue $ show y)
	, (monthMetaField, S.singleton $ toMetaValue $ show m)
	]
  where
	isnew (f, _) = S.null (currentMetaDataValues f old)
	(y, m, _d) = toGregorian $ utctDay $ 
		posixSecondsToUTCTime $ realToFrac $
			modificationTime status
