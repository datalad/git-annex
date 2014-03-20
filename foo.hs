{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist hiding (Key)
import Database.Persist.TH
import Database.Persist.Sqlite hiding (Key)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Data.Time.Clock
import Data.Maybe

import Types.Key
import Types.UUID
import Types.MetaData
import Footype

data RemoteFsckTime

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
CachedKey
  key Key
  KeyOutdex key
  deriving Show

AssociatedFiles
  keyId CachedKeyId Eq
  associatedFile FilePath
  KeyIdOutdex keyId associatedFile
  deriving Show

CachedMetaField
  field MetaField
  FieldOutdex field
  deriving Show

CachedMetaData
  keyId CachedKeyId Eq
  fieldId CachedMetaFieldId Eq
  metaValue String
  deriving Show

LastFscked
  keyId CachedKeyId Eq
  localFscked Int Maybe
|]

main :: IO ()
main = query

query :: IO ()
query = runSqlite "foo.db" $ do
	forM_ [1..1000] $ \i -> do
		Just k <- getBy $ KeyOutdex (fromJust $ file2key $ "WORM--" ++ show i)
		selectList [AssociatedFilesKeyId ==. entityKey k] []

query2 :: IO ()
query2 = runSqlite "foo.db" $ do
	forM_ [1..1] $ \i -> do
		Just f <- getBy $ FieldOutdex (fromJust $ toMetaField "tag")
		liftIO $ print f
		fs <- selectList [CachedMetaDataFieldId ==. entityKey f] []
		liftIO $ print $ length fs

populate :: IO ()
populate = runSqlite "foo.db"  $ do
    runMigration migrateAll
    t <- insert $ CachedMetaField (fromJust $ toMetaField "tag")
    f <- insert $ CachedMetaField (fromJust $ toMetaField "foo")
    forM_ [1..30000] $ \i -> do
    	k <- insert $ CachedKey (fromJust $ file2key $ "WORM--" ++ show i)
	liftIO $ print k
	insert $ AssociatedFiles k (show i)
	insert $ AssociatedFiles k ("and" ++ show (i + 1))
	insert $ CachedMetaData k f (show i)
	insert $ CachedMetaData k t "bar"
