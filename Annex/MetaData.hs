{- git-annex metadata
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.MetaData (
	genMetaData,
	dateMetaData,
	parseModMeta,
	parseMetaDataMatcher,
	module X
) where

import Annex.Common
import qualified Annex
import Types.MetaData as X
import Annex.MetaData.StandardFields as X
import Logs.MetaData
import Annex.CatFile
import Utility.Glob

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX

{- Adds metadata for a file that has just been ingested into the
 - annex, but has not yet been committed to git.
 -
 - When the file has been modified, the metadata is copied over
 - from the old key to the new key. Note that it looks at the old key as
 - committed to HEAD -- the new key may or may not have already been staged
 - in the index.
 -
 - Also, can generate new metadata, if configured to do so.
 -}
genMetaData :: Key -> FilePath -> FileStatus -> Annex ()
genMetaData key file status = do
	catKeyFileHEAD file >>= \case
		Nothing -> noop
		Just oldkey -> 
			whenM (copyMetaData oldkey key)
				warncopied
	whenM (annexGenMetaData <$> Annex.getGitConfig) $ do
		curr <- getCurrentMetaData key
		addMetaData key (dateMetaData mtime curr)
  where
	mtime = posixSecondsToUTCTime $ realToFrac $ modificationTime status
	warncopied = warning $ 
		"Copied metadata from old version of " ++ file ++ " to new version. " ++ 
		"If you don't want this copied metadata, run: git annex metadata --remove-all " ++ file

{- Generates metadata for a file's date stamp.
 - Does not overwrite any existing metadata values. -}
dateMetaData :: UTCTime -> MetaData -> MetaData
dateMetaData mtime old = MetaData $ M.fromList $ filter isnew
	[ (yearMetaField, S.singleton $ toMetaValue $ show y)
	, (monthMetaField, S.singleton $ toMetaValue $ show m)
	, (dayMetaField, S.singleton $ toMetaValue $ show d)
	]
  where
	isnew (f, _) = S.null (currentMetaDataValues f old)
	(y, m, d) = toGregorian $ utctDay mtime

{- Parses field=value, field+=value, field-=value, field?=value -}
parseModMeta :: String -> Either String ModMeta
parseModMeta p = case lastMaybe f of
	Just '+' -> AddMeta <$> mkMetaField f' <*> v
	Just '-' -> DelMeta <$> mkMetaField f' <*> (Just <$> v)
	Just '?' -> MaybeSetMeta <$> mkMetaField f' <*> v
	_ -> SetMeta <$> mkMetaField f <*> (S.singleton <$> v)
  where
	(f, sv) = separate (== '=') p
	f' = beginning f
	v = pure (toMetaValue sv)

{- Parses field=value, field<value, field<=value, field>value, field>=value -}
parseMetaDataMatcher :: String -> Either String (MetaField, MetaValue -> Bool)
parseMetaDataMatcher p = (,)
	<$> mkMetaField f
	<*> pure matcher
  where
	(f, op_v) = break (`elem` "=<>") p
	matcher = case op_v of
		('=':v) -> checkglob v
		('<':'=':v) -> checkcmp (<=) v
		('<':v) -> checkcmp (<) v
		('>':'=':v) -> checkcmp (>=) v
		('>':v) -> checkcmp (>) v
		_ -> checkglob ""
	checkglob v =
		let cglob = compileGlob v CaseInsensative
		in matchGlob cglob . fromMetaValue
	checkcmp cmp v v' = case (doubleval v, doubleval (fromMetaValue v')) of
		(Just d, Just d') -> d' `cmp` d
		_ -> False
	doubleval v = readish v :: Maybe Double
