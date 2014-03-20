{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell,
             OverloadedStrings, GADTs, FlexibleContexts #-}
import Database.Persist.TH
import Database.Persist.Sqlite (runSqlite)
import Control.Monad.IO.Class (liftIO)
import Control.Monad
import Database.Esqueleto hiding (Key)

share [mkPersist sqlSettings, mkSave "entityDefs", mkMigrate "migrateAll"] [persistLowerCase|
CachedKey
  key String
  UniqueKey key
  deriving Show

AssociatedFiles
  key CachedKeyId Eq
  file FilePath
  UniqueKeyFile key file
  deriving Show
|]

main :: IO ()
main = runSqlite "foo.db" $ do
	runMigration migrateAll

	forM_ [1..30000] $ \i -> do
		k <- insert $ CachedKey (show i)
		liftIO $ print k
		insert $ AssociatedFiles k (show i)

		[(k2)] <- select $ from $ \k -> do
			where_ (k ^. CachedKeyKey ==. val (show i))
			return (k ^. CachedKeyId)
		liftIO $ print (2, k2)
		delete $ from $ \f -> do
			where_ (f ^. AssociatedFilesKey ==. k2)
