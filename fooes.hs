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
	if True then populate else return ()
	query

populate = do
	forM_ [1..30000] $ \i -> do
		--delete $ from $ \f -> do
		--	where_ (f ^. CachedKeyKey ==. val (show i))
		k <- insert $ CachedKey (show i)
		liftIO $ print ("stored", k)
		insert $ AssociatedFiles k ("file" ++show (i+1))
		--insert $ AssociatedFiles k ("otherfile" ++show (i+2))

query = forM_ [1..1000] $ \i -> do
	r <- select $ from $ \(k, f) -> do
		where_ (k ^. CachedKeyKey ==. val (show i))
		where_ (f ^. AssociatedFilesKey ==. k ^. CachedKeyId)
		return (f ^. AssociatedFilesFile)
	liftIO $ print ("got", r)
