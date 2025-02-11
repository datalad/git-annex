{- Sqlite database of known urls, and another of known itemids,
 - for use by git-annex importfeed.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -:
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE QuasiQuotes, TypeFamilies, TypeOperators, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.ImportFeed (
	ImportFeedDbHandle,
	openDb,
	closeDb,
	isKnownUrl,
	isKnownItemId,
) where

import Database.Types
import qualified Database.Queue as H
import Database.Init
import Database.Utility
import Annex.Locations
import Annex.Common hiding (delete)
import qualified Annex.Branch
import Git.Types
import Git.Sha
import Git.FilePath
import qualified Git.DiffTree as DiffTree
import Logs
import Logs.Web
import Logs.MetaData
import Types.MetaData
import Annex.MetaData.StandardFields
import Annex.LockFile

import Database.Persist.Sql hiding (Key)
import Database.Persist.TH
import qualified Data.ByteString as B
import qualified Data.Set as S

data ImportFeedDbHandle = ImportFeedDbHandle H.DbQueue

-- Note on indexes: ContentIndentifiersKeyRemoteCidIndex etc are really
-- uniqueness constraints, which cause sqlite to automatically add indexes.
-- So when adding indexes, have to take care to only add ones that work as
-- uniqueness constraints. (Unfortunately persistent does not support indexes
-- that are not uniqueness constraints; 
-- https://github.com/yesodweb/persistent/issues/109)
share [mkPersist sqlSettings, mkMigrate "migrateImportFeed"] [persistLowerCase|
KnownUrls
  url SByteString
  UniqueUrl url
KnownItemIds
  itemid SByteString
  UniqueItemId itemid
-- The last git-annex branch tree sha that was used to update
-- KnownUrls and KnownItemIds
AnnexBranch
  tree SSha
  UniqueTree tree
|]

{- Opens the database, creating it if it doesn't exist yet.
 - Updates the database from the git-annex branch. -}
openDb :: Annex ImportFeedDbHandle
openDb = do
	dbdir <- calcRepo' gitAnnexImportFeedDbDir
	let db = dbdir </> literalOsPath "db"
	isnew <- liftIO $ not <$> doesFileExist db
	when isnew $
		initDb db $ void $ 
			runMigrationSilent migrateImportFeed
	dbh <- liftIO $ H.openDbQueue db "known_urls"
	let h = ImportFeedDbHandle dbh
	needsUpdateFromLog h >>= \case
		Nothing -> return ()
		Just v -> do
			lck <- calcRepo' gitAnnexImportFeedDbLock
                	withExclusiveLock lck $
				updateFromLog h v
	return h

closeDb :: ImportFeedDbHandle -> Annex ()
closeDb (ImportFeedDbHandle h) = liftIO $ H.closeDbQueue h

isKnownUrl :: ImportFeedDbHandle -> URLString -> IO Bool
isKnownUrl (ImportFeedDbHandle h) u = 
	H.queryDbQueue h $ do
		l <- selectList
			[ KnownUrlsUrl ==. SByteString (encodeBS u)
			] []
		return $ not (null l)

isKnownItemId :: ImportFeedDbHandle -> B.ByteString -> IO Bool
isKnownItemId (ImportFeedDbHandle h) i = 
	H.queryDbQueue h $ do
		l <- selectList
			[ KnownItemIdsItemid ==. SByteString i
			] []
		return $ not (null l)

recordKnownUrl :: ImportFeedDbHandle -> URLByteString -> IO ()
recordKnownUrl h u = queueDb h $
	void $ insertUniqueFast $ KnownUrls $ SByteString u

recordKnownItemId :: ImportFeedDbHandle -> SByteString -> IO ()
recordKnownItemId h i = queueDb h $
	void $ insertUniqueFast $ KnownItemIds i

recordAnnexBranchTree :: ImportFeedDbHandle -> Sha -> IO ()
recordAnnexBranchTree h s = queueDb h $ do
	deleteWhere ([] :: [Filter AnnexBranch])
	void $ insertUniqueFast $ AnnexBranch $ toSSha s

getAnnexBranchTree :: ImportFeedDbHandle -> IO Sha
getAnnexBranchTree (ImportFeedDbHandle h) = H.queryDbQueue h $ do
	l <- selectList ([] :: [Filter AnnexBranch]) []
	case l of
		(s:[]) -> return $ fromSSha $ annexBranchTree $ entityVal s
		_ -> return emptyTree

queueDb :: ImportFeedDbHandle -> SqlPersistM () -> IO ()
queueDb (ImportFeedDbHandle h) = H.queueDb h checkcommit
  where
        -- commit queue after 10000 changes
        checkcommit sz _lastcommittime
                | sz > 10000 = return True
                | otherwise = return False

{- Check if the git-annex branch has been updated and the database needs
 - to be updated with any new information from it. -}
needsUpdateFromLog :: ImportFeedDbHandle -> Annex (Maybe (Sha, Sha))
needsUpdateFromLog db = do
	oldtree <- liftIO $ getAnnexBranchTree db
	Annex.Branch.updatedFromTree oldtree

{- The database should be locked for write when calling this. -}
updateFromLog :: ImportFeedDbHandle -> (Sha, Sha) -> Annex ()
updateFromLog db@(ImportFeedDbHandle h) (oldtree, currtree)
	| oldtree == emptyTree = do
		scanbranch
		out
	| otherwise = do
		scandiff
		out
  where
  	out = liftIO $ do
		recordAnnexBranchTree db currtree
		H.flushDbQueue h
	
	knownitemids s = liftIO $ forM_ (S.toList s) $
		recordKnownItemId db . SByteString . fromMetaValue

	knownurls us = liftIO $ forM_ us $
		recordKnownUrl db
		
	scandiff = do
		(l, cleanup) <- inRepo $
			DiffTree.diffTreeRecursive oldtree currtree
		mapM_ godiff l
		void $ liftIO $ cleanup
	
	godiff ti = do
		let f = getTopFilePath (DiffTree.file ti)
		case extLogFileKey urlLogExt f of
			Just k -> do
				knownurls =<< getUrls' k
			Nothing -> case extLogFileKey metaDataLogExt f of
				Just k -> do
					m <- getCurrentMetaData k
					knownitemids (currentMetaDataValues itemIdField m)
				Nothing -> return ()

	-- When initially populating the database, this 
	-- is faster than diffing from the empty tree
	-- and looking up every log file.
	scanbranch = Annex.Branch.overBranchFileContents False toscan goscan >>= \case
		Annex.Branch.NoUnmergedBranches ((), _) -> return ()
		Annex.Branch.UnmergedBranches ((), _) -> scandiff
	
	toscan f
		| isUrlLog f = Just ()
		| isMetaDataLog f = Just ()
		| otherwise = Nothing
	
	goscan reader = reader >>= \case
		Just ((), f, Just (content, _))
			| isUrlLog f -> do
				knownurls (parseUrlLog content)
				goscan reader
			| isMetaDataLog f -> do
				knownitemids $
					currentMetaDataValues itemIdField $
						parseCurrentMetaData content
				goscan reader
			| otherwise -> goscan reader
		Just ((), _, Nothing) -> goscan reader
		Nothing -> return ()
