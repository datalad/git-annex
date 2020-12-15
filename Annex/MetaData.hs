{- git-annex metadata
 -
 - Copyright 2014-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import qualified Data.Text as T
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
genMetaData :: Key -> RawFilePath -> FileStatus -> Annex ()
genMetaData key file status = do
	catKeyFileHEAD file >>= \case
		Nothing -> noop
		Just oldkey ->
			-- Have to copy first, before adding any
			-- more metadata, because copyMetaData does not
			-- preserve any metadata already on key.
			whenM (copyMetaData oldkey key <&&> (not <$> onlydatemeta oldkey)) $
				warncopied
	whenM (annexGenMetaData <$> Annex.getGitConfig) $ do
		old <- getCurrentMetaData key
		addMetaData key (dateMetaData mtime old)
  where
	mtime = posixSecondsToUTCTime $ realToFrac $ modificationTime status
	warncopied = warning $ 
		"Copied metadata from old version of " ++ fromRawFilePath file ++ " to new version. " ++ 
		"If you don't want this copied metadata, run: git annex metadata --remove-all " ++ fromRawFilePath file
	-- If the only fields copied were date metadata, and they'll
	-- be overwritten with the current mtime, no need to warn about
	-- copying.
	onlydatemeta oldkey = ifM (annexGenMetaData <$> Annex.getGitConfig)
		( null . filter (not . isDateMetaField . fst) . fromMetaData 
			<$> getCurrentMetaData oldkey
		, return False
		)

{- Generates metadata for a file's date stamp.
 -
 - Any date fields in the old metadata will be overwritten.
 - 
 - Note that the returned MetaData does not contain all the input MetaData,
 - only changes to add the date fields. -}
dateMetaData :: UTCTime -> MetaData -> MetaData
dateMetaData mtime old = modMeta old $
	(SetMeta yearMetaField $ S.singleton $ toMetaValue $ encodeBS' $ show y)
		`ComposeModMeta`
	(SetMeta monthMetaField $ S.singleton $ toMetaValue $ encodeBS' $ show m)
		`ComposeModMeta`
	(SetMeta dayMetaField $ S.singleton $ toMetaValue $ encodeBS' $ show d)
  where
	(y, m, d) = toGregorian $ utctDay mtime

{- Parses field=value, field+=value, field-=value, field?=value -}
parseModMeta :: String -> Either String ModMeta
parseModMeta p = case lastMaybe f of
	Just '+' -> AddMeta <$> mkMetaField (T.pack f') <*> v
	Just '-' -> DelMeta <$> mkMetaField (T.pack f') <*> (Just <$> v)
	Just '?' -> MaybeSetMeta <$> mkMetaField (T.pack f') <*> v
	_ -> SetMeta <$> mkMetaField (T.pack f) <*> (S.singleton <$> v)
  where
	(f, sv) = separate (== '=') p
	f' = beginning f
	v = pure (toMetaValue (encodeBS sv))

{- Parses field=value, field<value, field<=value, field>value, field>=value -}
parseMetaDataMatcher :: String -> Either String (MetaField, MetaValue -> Bool)
parseMetaDataMatcher p = (,)
	<$> mkMetaField (T.pack f)
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
		let cglob = compileGlob v CaseInsensative (GlobFilePath False)
		in matchGlob cglob . decodeBS . fromMetaValue
	checkcmp cmp v v' = case (doubleval v, doubleval (decodeBS (fromMetaValue v'))) of
		(Just d, Just d') -> d' `cmp` d
		_ -> False
	doubleval v = readish v :: Maybe Double
