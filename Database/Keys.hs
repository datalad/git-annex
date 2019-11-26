{- Sqlite database of information about Keys
 -
 - Copyright 2015-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}

module Database.Keys (
	DbHandle,
	closeDb,
	addAssociatedFile,
	getAssociatedFiles,
	getAssociatedKey,
	removeAssociatedFile,
	storeInodeCaches,
	storeInodeCaches',
	addInodeCaches,
	getInodeCaches,
	removeInodeCaches,
	isInodeKnown,
	runWriter,
) where

import qualified Database.Keys.SQL as SQL
import Database.Types
import Database.Keys.Handle
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common hiding (delete)
import qualified Annex
import Annex.LockFile
import Annex.CatFile
import Annex.Content.PointerFile
import Annex.Link
import Utility.InodeCache
import Annex.InodeSentinal
import Git
import Git.FilePath
import Git.Command
import Git.Types
import Git.Index

{- Runs an action that reads from the database.
 -
 - If the database doesn't already exist, it's not created; mempty is
 - returned instead. This way, when the keys database is not in use,
 - there's minimal overhead in checking it.
 -
 - If the database is already open, any writes are flushed to it, to ensure
 - consistency.
 -
 - Any queued writes will be flushed before the read.
 -}
runReader :: Monoid v => (SQL.ReadHandle -> Annex v) -> Annex v
runReader a = do
	h <- getDbHandle
	withDbState h go
  where
	go DbUnavailable = return (mempty, DbUnavailable)
	go st@(DbOpen qh) = do
		liftIO $ H.flushDbQueue qh
		v <- a (SQL.ReadHandle qh)
		return (v, st)
	go DbClosed = do
		st' <- openDb False DbClosed
		v <- case st' of
			(DbOpen qh) -> a (SQL.ReadHandle qh)
			_ -> return mempty
		return (v, st')

runReaderIO :: Monoid v => (SQL.ReadHandle -> IO v) -> Annex v
runReaderIO a = runReader (liftIO . a)

{- Runs an action that writes to the database. Typically this is used to
 - queue changes, which will be flushed at a later point.
 -
 - The database is created if it doesn't exist yet. -}
runWriter :: (SQL.WriteHandle -> Annex ()) -> Annex ()
runWriter a = do
	h <- getDbHandle
	withDbState h go
  where
	go st@(DbOpen qh) = do
		v <- a (SQL.WriteHandle qh)
		return (v, st)
	go st = do
		st' <- openDb True st
		v <- case st' of
			DbOpen qh -> a (SQL.WriteHandle qh)
			_ -> error "internal"
		return (v, st')

runWriterIO :: (SQL.WriteHandle -> IO ()) -> Annex ()
runWriterIO a = runWriter (liftIO . a)

{- Gets the handle cached in Annex state; creates a new one if it's not yet
 - available, but doesn't open the database. -}
getDbHandle :: Annex DbHandle
getDbHandle = go =<< Annex.getState Annex.keysdbhandle
  where
	go (Just h) = pure h
	go Nothing = do
		h <- liftIO newDbHandle
		Annex.changeState $ \s -> s { Annex.keysdbhandle = Just h }
		return h

{- Opens the database, perhaps creating it if it doesn't exist yet.
 -
 - Multiple readers and writers can have the database open at the same
 - time. Database.Handle deals with the concurrency issues.
 - The lock is held while opening the database, so that when
 - the database doesn't exist yet, one caller wins the lock and
 - can create it undisturbed.
 -}
openDb :: Bool -> DbState -> Annex DbState
openDb _ st@(DbOpen _) = return st
openDb False DbUnavailable = return DbUnavailable
openDb createdb _ = catchPermissionDenied permerr $ withExclusiveLock gitAnnexKeysDbLock $ do
	dbdir <- fromRepo gitAnnexKeysDb
	let db = dbdir </> "db"
	dbexists <- liftIO $ doesFileExist db
	case (dbexists, createdb) of
		(True, _) -> open db
		(False, True) -> do
			initDb db SQL.createTables
			open db
		(False, False) -> return DbUnavailable
  where
	-- If permissions don't allow opening the database, treat it as if
	-- it does not exist.
	permerr e = case createdb of
		False -> return DbUnavailable
		True -> throwM e
	
	open db = do
		qh <- liftIO $ H.openDbQueue H.MultiWriter db SQL.containedTable
		reconcileStaged qh
		return $ DbOpen qh

{- Closes the database if it was open. Any writes will be flushed to it.
 -
 - This does not normally need to be called; the database will auto-close
 - when the handle is garbage collected. However, this can be used to
 - force a re-read of the database, in case another process has written
 - data to it.
 -}
closeDb :: Annex ()
closeDb = Annex.getState Annex.keysdbhandle >>= \case
	Nothing -> return ()
	Just h -> liftIO (closeDbHandle h)

addAssociatedFile :: Key -> TopFilePath -> Annex ()
addAssociatedFile k f = runWriterIO $ SQL.addAssociatedFile (toIKey k) f

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> Annex [TopFilePath]
getAssociatedFiles = runReaderIO . SQL.getAssociatedFiles . toIKey

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> Annex [Key]
getAssociatedKey = map fromIKey <$$> runReaderIO . SQL.getAssociatedKey

removeAssociatedFile :: Key -> TopFilePath -> Annex ()
removeAssociatedFile k = runWriterIO . SQL.removeAssociatedFile (toIKey k)

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [FilePath] -> Annex ()
storeInodeCaches k fs = storeInodeCaches' k fs []

storeInodeCaches' :: Key -> [FilePath] -> [InodeCache] -> Annex ()
storeInodeCaches' k fs ics = withTSDelta $ \d ->
	addInodeCaches k . (++ ics) . catMaybes
		=<< liftIO (mapM (`genInodeCache` d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = runWriterIO $ SQL.addInodeCaches (toIKey k) is

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it. -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches = runReaderIO . SQL.getInodeCaches . toIKey

removeInodeCaches :: Key -> Annex ()
removeInodeCaches = runWriterIO . SQL.removeInodeCaches . toIKey

isInodeKnown :: InodeCache -> SentinalStatus -> Annex Bool
isInodeKnown i s = or <$> runReaderIO ((:[]) <$$> SQL.isInodeKnown i s)

{- Looks at staged changes to find when unlocked files are copied/moved,
 - and updates associated files in the keys database.
 -
 - Since staged changes can be dropped later, does not remove any
 - associated files; only adds new associated files.
 -
 - This needs to be run before querying the keys database so that
 - information is consistent with the state of the repository.
 -
 - To avoid unncessary work, the index file is statted, and if it's not
 - changed since last time this was run, nothing is done.
 -
 - Note that this is run with a lock held, so only one process can be
 - running this at a time.
 -
 - This also cleans up after a race between eg a git mv and git-annex
 - get/drop/similar. If git moves the file between this being run and the
 - get/drop, the moved file won't be updated for the get/drop. 
 - The next time this runs, it will see the staged change. It then checks
 - if the worktree file's content availability does not match the git-annex
 - content availablity, and makes changes as necessary to reconcile them.
 -
 - Note that if a commit happens before this runs again, it won't see
 - the staged change. Instead, during the commit, git will run the clean
 - filter. If a drop missed the file then the file is added back into the
 - annex. If a get missed the file then the clean filter populates the
 - file.
 -}
reconcileStaged :: H.DbQueue -> Annex ()
reconcileStaged qh = do
	gitindex <- inRepo currentIndexFile
	indexcache <- fromRepo gitAnnexKeysDbIndexCache
	withTSDelta (liftIO . genInodeCache gitindex) >>= \case
		Just cur -> 
			liftIO (maybe Nothing readInodeCache <$> catchMaybeIO (readFile indexcache)) >>= \case
				Nothing -> go cur indexcache
				Just prev -> ifM (compareInodeCaches prev cur)
					( noop
					, go cur indexcache
					)
		Nothing -> noop
  where
	go cur indexcache = do
		(l, cleanup) <- inRepo $ pipeNullSplit diff
		changed <- procdiff (map decodeBL' l) False
		void $ liftIO cleanup
		-- Flush database changes immediately
		-- so other processes can see them.
		when changed $
			liftIO $ H.flushDbQueue qh
		liftIO $ writeFile indexcache $ showInodeCache cur
	
	diff =
		-- Avoid using external diff command, which would be slow.
		-- (The -G option may make it be used otherwise.)
		[ Param "-c", Param "diff.external="
		-- Avoid running smudge or clean filters, since we want the
		-- raw output, and they would block trying to access the
		-- locked database. The --raw normally avoids git diff
		-- running them, but older versions of git need this.
		, Param "-c", Param "filter.annex.smudge="
		, Param "-c", Param "filter.annex.clean="
		, Param "diff"
		, Param "--cached"
		, Param "--raw"
		, Param "-z"
		, Param "--abbrev=40"
		-- Optimization: Only find pointer files. This is not
		-- perfect. A file could start with this and not be a
		-- pointer file. And a pointer file that is replaced with
		-- a non-pointer file will match this.
		, Param $ "-G^" ++ fromRawFilePath (toInternalGitPath $
			toRawFilePath (pathSeparator:objectDir))
		-- Don't include files that were deleted, because this only
		-- wants to update information for files that are present
		-- in the index.
		, Param "--diff-filter=AMUT"
		-- Disable rename detection.
		, Param "--no-renames"
		-- Avoid other complications.
		, Param "--ignore-submodules=all"
		, Param "--no-ext-diff"
		]
	
	procdiff (info:file:rest) changed = case words info of
		((':':_srcmode):dstmode:_srcsha:dstsha:_change:[])
			-- Only want files, not symlinks
			| dstmode /= decodeBS' (fmtTreeItemType TreeSymlink) -> do
				maybe noop (reconcile (asTopFilePath file)) 
					=<< catKey (Ref dstsha)
				procdiff rest True
			| otherwise -> procdiff rest changed
		_ -> return changed -- parse failed
	procdiff _ changed = return changed

	-- Note that database writes done in here will not necessarily 
	-- be visible to database reads also done in here.
	reconcile file key = do
		let ikey = toIKey key
		liftIO $ SQL.addAssociatedFileFast ikey file (SQL.WriteHandle qh)
		caches <- liftIO $ SQL.getInodeCaches ikey (SQL.ReadHandle qh)
		keyloc <- calcRepo (gitAnnexLocation key)
		keypopulated <- sameInodeCache keyloc caches
		p <- fromRepo $ toRawFilePath . fromTopFilePath file
		filepopulated <- sameInodeCache (fromRawFilePath p) caches
		case (keypopulated, filepopulated) of
			(True, False) ->
				populatePointerFile (Restage True) key (toRawFilePath keyloc) p >>= \case
					Nothing -> return ()
					Just ic -> liftIO $
						SQL.addInodeCaches ikey [ic] (SQL.WriteHandle qh)
			(False, True) -> depopulatePointerFile key p
			_ -> return ()
