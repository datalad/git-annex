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
import Annex.CatFile

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

{- Adds metadata for a file that has just been ingested into the
 - annex, but has not yet been committed to git.
 -
 - When the file has been modified, the metadata is copied over
 - from the old key to the new key. Note that it looks at the old key as
 - committed to HEAD -- the new key may or may not have already been staged
 - in th annex.
 -
 - Also, can generate new metadata, if configured to do so.
 -}
genMetaData :: Key -> FilePath -> FileStatus -> Annex ()
genMetaData key file status = do
	maybe noop (flip copyMetaData key) =<< catKeyFileHEAD file
	whenM (annexGenMetaData <$> Annex.getGitConfig) $ do
		metadata <- getCurrentMetaData key
		let metadata' = genMetaData' status metadata
		unless (metadata' == emptyMetaData) $
			addMetaData key metadata'

{- Generates metadata from the FileStatus.
 - Does not overwrite any existing metadata values. -}
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
