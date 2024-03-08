{- Sqlite database of information about Keys
 -
 - Copyright 2015-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Database.Keys (
	DbHandle,
	closeDb,
	flushDb,
	addAssociatedFile,
	getAssociatedFiles,
	getAssociatedFilesIncluding,
	getAssociatedKey,
	removeAssociatedFile,
	storeInodeCaches,
	addInodeCaches,
	getInodeCaches,
	removeInodeCaches,
	removeInodeCache,
	isInodeKnown,
	runWriter,
	updateDatabase,
) where

import qualified Database.Keys.SQL as SQL
import Database.Types
import Database.Keys.Handle
import Database.Keys.Tables
import qualified Database.Queue as H
import Database.Init
import Annex.Locations
import Annex.Common hiding (delete)
import qualified Annex
import Annex.LockFile
import Annex.Content.PointerFile
import Annex.Content.Presence.LowLevel
import Annex.Link (Restage(..), maxPointerSz, parseLinkTargetOrPointerLazy)
import Utility.InodeCache
import Annex.InodeSentinal
import Git
import Git.FilePath
import Git.Command
import Git.Types
import Git.Index
import Git.Sha
import Git.CatFile
import Git.Branch (writeTreeQuiet, update')
import qualified Git.Ref
import Config
import Config.Smudge
import qualified Utility.RawFilePath as R

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified System.FilePath.ByteString as P
import Control.Concurrent.Async

{- Runs an action that reads from the database.
 -
 - If the database is already open, any writes are flushed to it, to ensure
 - consistency.
 -
 - Any queued writes to the table will be flushed before the read.
 -}
runReader :: Monoid v => DbTable -> (SQL.ReadHandle -> Annex v) -> Annex v
runReader t a = do
	h <- Annex.getRead Annex.keysdbhandle
	withDbState h go
  where
	go DbUnavailable = return (mempty, DbUnavailable)
	go (DbOpen (qh, tableschanged)) = do
		tableschanged' <- if isDbTableChanged tableschanged t
			then do
				liftIO $ H.flushDbQueue qh
				return mempty
			else return tableschanged
		v <- a (SQL.ReadHandle qh)
		return (v, DbOpen (qh, tableschanged'))
	go DbClosed = do
		st <- openDb False DbClosed
		v <- case st of
			(DbOpen (qh, _)) -> a (SQL.ReadHandle qh)
			_ -> return mempty
		return (v, st)

runReaderIO :: Monoid v => DbTable -> (SQL.ReadHandle -> IO v) -> Annex v
runReaderIO t a = runReader t (liftIO . a)

{- Runs an action that writes to the database. Typically this is used to
 - queue changes, which will be flushed at a later point.
 -
 - The database is created if it doesn't exist yet. -}
runWriter :: DbTable -> (SQL.WriteHandle -> Annex ()) -> Annex ()
runWriter t a = do
	h <- Annex.getRead Annex.keysdbhandle
	withDbState h go
  where
	go (DbOpen (qh, tableschanged)) = do
		v <- a (SQL.WriteHandle qh)
		return (v, DbOpen (qh, addDbTable tableschanged t))
	go st = do
		st' <- openDb True st
		v <- case st' of
			DbOpen (qh, _) -> a (SQL.WriteHandle qh)
			_ -> error "internal"
		return (v, st')

runWriterIO :: DbTable -> (SQL.WriteHandle -> IO ()) -> Annex ()
runWriterIO t a = runWriter t (liftIO . a)

{- Opens the database, creating it if it doesn't exist yet.
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
openDb forwrite _ = do
	lck <- calcRepo' gitAnnexKeysDbLock
	catchPermissionDenied permerr $ withExclusiveLock lck $ do
		dbdir <- calcRepo' gitAnnexKeysDbDir
		let db = dbdir P.</> "db"
		dbexists <- liftIO $ R.doesPathExist db
		case dbexists of
			True -> open db False
			False -> do
				initDb db SQL.createTables
				open db True
  where
	-- If permissions don't allow opening the database, and it's being
	-- opened for read, treat it as if it does not exist.
	permerr e
		| forwrite = throwM e
		| otherwise = return DbUnavailable
	
	open db dbisnew = do
		qh <- liftIO $ H.openDbQueue db SQL.containedTable
		tc <- reconcileStaged dbisnew qh
		return $ DbOpen (qh, tc)

{- Closes the database if it was open. Any writes will be flushed to it.
 -
 - This does not prevent further use of the database; it will be re-opened
 - as necessary.
 -}
closeDb :: Annex ()
closeDb = liftIO . closeDbHandle =<< Annex.getRead Annex.keysdbhandle

{- Flushes any queued writes to the database. -}
flushDb :: Annex ()
flushDb = liftIO . flushDbQueue =<< Annex.getRead Annex.keysdbhandle

addAssociatedFile :: Key -> TopFilePath -> Annex ()
addAssociatedFile k f = runWriterIO AssociatedTable $ SQL.addAssociatedFile k f

{- Note that the files returned were once associated with the key, but
 - some of them may not be any longer. -}
getAssociatedFiles :: Key -> Annex [TopFilePath]
getAssociatedFiles k = emptyWhenBare $ runReaderIO AssociatedTable $
	SQL.getAssociatedFiles k

{- Queries for associated files never return anything when in a bare
 - repository, since without a work tree there can be no associated files. 
 -
 - Normally the keys database is not even populated with associated files 
 - in a bare repository, but it might happen if a non-bare repo got
 - converted to bare. -}
emptyWhenBare :: Annex [a] -> Annex [a]
emptyWhenBare a = ifM isBareRepo
	( return []
	, a
	)

{- Include a known associated file along with any recorded in the database. -}
getAssociatedFilesIncluding :: AssociatedFile -> Key -> Annex [RawFilePath]
getAssociatedFilesIncluding afile k = emptyWhenBare $ do
	g <- Annex.gitRepo
	l <- map (`fromTopFilePath` g) <$> getAssociatedFiles k
	return $ case afile of
		AssociatedFile (Just f) -> f : filter (/= f) l
		AssociatedFile Nothing -> l

{- Gets any keys that are on record as having a particular associated file.
 - (Should be one or none but the database doesn't enforce that.) -}
getAssociatedKey :: TopFilePath -> Annex [Key]
getAssociatedKey f = emptyWhenBare $ runReaderIO AssociatedTable $
	SQL.getAssociatedKey f

removeAssociatedFile :: Key -> TopFilePath -> Annex ()
removeAssociatedFile k = runWriterIO AssociatedTable .
	SQL.removeAssociatedFile k

{- Stats the files, and stores their InodeCaches. -}
storeInodeCaches :: Key -> [RawFilePath] -> Annex ()
storeInodeCaches k fs = withTSDelta $ \d ->
	addInodeCaches k . catMaybes
		=<< liftIO (mapM (\f -> genInodeCache f d) fs)

addInodeCaches :: Key -> [InodeCache] -> Annex ()
addInodeCaches k is = runWriterIO ContentTable $ SQL.addInodeCaches k is

{- A key may have multiple InodeCaches; one for the annex object, and one
 - for each pointer file that is a copy of it.
 -
 - When there are no pointer files, the annex object typically does not
 - have its InodeCache recorded either, so the list will be empty.
 -
 - Note that, in repos upgraded from v7, there may be InodeCaches recorded
 - for pointer files, but none recorded for the annex object.
 -}
getInodeCaches :: Key -> Annex [InodeCache]
getInodeCaches = runReaderIO ContentTable . SQL.getInodeCaches

{- Remove all inodes cached for a key. -}
removeInodeCaches :: Key -> Annex ()
removeInodeCaches = runWriterIO ContentTable . SQL.removeInodeCaches

{- Remove cached inodes, for any key. -}
removeInodeCache :: InodeCache -> Annex ()
removeInodeCache = runWriterIO ContentTable . SQL.removeInodeCache

isInodeKnown :: InodeCache -> SentinalStatus -> Annex Bool
isInodeKnown i s = or <$> runReaderIO ContentTable 
	((:[]) <$$> SQL.isInodeKnown i s)

{- Looks at staged changes to annexed files, and updates the keys database,
 - so that its information is consistent with the state of the repository.
 -
 - This is run with a lock held, so only one process can be running this at
 - a time.
 -
 - To avoid unnecessary work, the index file is statted, and if it's not
 - changed since last time this was run, nothing is done.
 -
 - A tree is generated from the index, and the diff between that tree
 - and the last processed tree is examined for changes.
 -
 - This also cleans up after a race between eg a git mv and git-annex
 - get/drop/similar. If git moves a pointer file between this being run and the
 - get/drop, the moved pointer file won't be updated for the get/drop. 
 - The next time this runs, it will see the staged change. It then checks
 - if the pointer file needs to be updated to contain or not contain the
 - annex content.
 -
 - Note: There is a situation where, after this has run, the database can
 - still contain associated files that have been deleted from the index.
 - That happens when addAssociatedFile is used to record a newly
 - added file, but that file then gets removed from the index before
 - this is run. Eg, "git-annex add foo; git rm foo"
 - So when using getAssociatedFiles, have to make sure the file still
 - is an associated file.
 -}
reconcileStaged :: Bool -> H.DbQueue -> Annex DbTablesChanged
reconcileStaged dbisnew qh = ifM isBareRepo
	( return mempty
	, do
		gitindex <- inRepo currentIndexFile
		indexcache <- fromRawFilePath <$> calcRepo' gitAnnexKeysDbIndexCache
		withTSDelta (liftIO . genInodeCache gitindex) >>= \case
			Just cur -> readindexcache indexcache >>= \case
				Nothing -> go cur indexcache =<< getindextree
				Just prev -> ifM (compareInodeCaches prev cur)
					( return mempty
					, go cur indexcache =<< getindextree
					)
			Nothing -> return mempty
	)
  where
	lastindexref = Ref "refs/annex/last-index"

	readindexcache indexcache = liftIO $ maybe Nothing readInodeCache
		<$> catchMaybeIO (readFile indexcache)

	getoldtree = fromMaybe emptyTree <$> inRepo (Git.Ref.sha lastindexref)
	
	go cur indexcache (Just newtree) = do
		oldtree <- getoldtree
		when (oldtree /= newtree) $ do
			fastDebug "Database.Keys" "reconcileStaged start"
			g <- Annex.gitRepo
			void $ catstream $ \mdfeeder -> 
				void $ updatetodiff g
					(Just (fromRef oldtree)) 
					(fromRef newtree)
					(procdiff mdfeeder)
			liftIO $ writeFile indexcache $ showInodeCache cur
			-- Storing the tree in a ref makes sure it does not
			-- get garbage collected, and is available to diff
			-- against next time.
			inRepo $ update' lastindexref newtree
			fastDebug "Database.Keys" "reconcileStaged end"
		return (DbTablesChanged True True)
	-- git write-tree will fail if the index is locked or when there is
	-- a merge conflict. To get up-to-date with the current index, 
	-- diff --staged with the old index tree. The current index tree
	-- is not known, so not recorded, and the inode cache is not updated,
	-- so the next time git-annex runs, it will diff again, even
	-- if the index is unchanged.
	--
	-- When there is a merge conflict, that will not see the new local
	-- version of the files that are conflicted. So a second diff
	-- is done, with --staged but no old tree.
	go _ _ Nothing = do
		fastDebug "Database.Keys" "reconcileStaged start (in conflict)"
		oldtree <- getoldtree
		g <- Annex.gitRepo
		catstream $ \mdfeeder -> do
			conflicted <- updatetodiff g
				(Just (fromRef oldtree)) "--staged" (procdiff mdfeeder)
			when conflicted $
				void $ updatetodiff g Nothing "--staged"
					(procmergeconflictdiff mdfeeder)
		fastDebug "Database.Keys" "reconcileStaged end"
		return (DbTablesChanged True True)
	
	updatetodiff g old new processor = do
		(l, cleanup) <- pipeNullSplit' (diff old new) g
		processor l False
			`finally` void cleanup
	
	-- Avoid running smudge clean filter, which would block trying to
	-- access the locked database. git write-tree sometimes calls it,
	-- even though it is not adding work tree files to the index,
	-- and so the filter cannot have an effect on the contents of the
	-- index or on the tree that gets written from it.
	getindextree = inRepo $ \r -> writeTreeQuiet $ r
		{ gitGlobalOpts = gitGlobalOpts r ++ bypassSmudgeConfig }
	
	diff old new =
		-- Avoid running smudge clean filter, since we want the
		-- raw output, and it would block trying to access the
		-- locked database. The --raw normally avoids git diff
		-- running them, but older versions of git need this.
		bypassSmudgeConfig ++
		-- Avoid using external diff command, which would be slow.
		-- (The -G option may make it be used otherwise.)
		[ Param "-c", Param "diff.external="
		, Param "diff"
		] ++ maybeToList (Param <$> old) ++
		[ Param new
		, Param "--raw"
		, Param "-z"
		, Param "--no-abbrev"
		-- Optimization: Limit to pointer files and annex symlinks.
		-- This is not perfect. A file could contain this and not
		-- be a pointer file. And a pointer file that is replaced with
		-- a non-pointer file will match this. This is only a
		-- prefilter so that's ok.
		, Param $ "-G" ++ fromRawFilePath (toInternalGitPath $
			P.pathSeparator `S.cons` objectDir)
		-- Disable rename detection.
		, Param "--no-renames"
		-- Avoid other complications.
		, Param "--ignore-submodules=all"
		-- Avoid using external textconv command, which would be slow
		-- and possibly wrong.
		, Param "--no-textconv"
		, Param "--no-ext-diff"
		]
	
	procdiff mdfeeder (info:file:rest) conflicted
		| ":" `S.isPrefixOf` info = case S8.words info of
			(_colonsrcmode:dstmode:srcsha:dstsha:status:[]) -> do
				let conflicted' = status == "U"
				-- avoid removing associated file when
				-- there is a merge conflict
				unless conflicted' $
					send mdfeeder (Ref srcsha) $ \case
						Just oldkey -> do
							liftIO $ SQL.removeAssociatedFile oldkey
								(asTopFilePath file)
								(SQL.WriteHandle qh)
							return True
						Nothing -> return False
				send mdfeeder (Ref dstsha) $ \case
					Just key -> do
						liftIO $ addassociatedfile key
							(asTopFilePath file)
							(SQL.WriteHandle qh)
						when (dstmode /= fmtTreeItemType TreeSymlink) $
							reconcilepointerfile (asTopFilePath file) key
						return True
					Nothing -> return False
				procdiff mdfeeder rest
					(conflicted || conflicted')
			_ -> return conflicted -- parse failed
	procdiff _ _ conflicted = return conflicted

	-- Processing a diff --index when there is a merge conflict.
	-- This diff will have the new local version of a file as the
	-- first sha, and a null sha as the second sha, and we only
	-- care about files that are in conflict.
	procmergeconflictdiff mdfeeder (info:file:rest) conflicted
		| ":" `S.isPrefixOf` info = case S8.words info of
			(_colonmode:_mode:sha:_sha:status:[]) -> do
				send mdfeeder (Ref sha) $ \case
					Just key -> do
						liftIO $ SQL.addAssociatedFile key
							(asTopFilePath file)
							(SQL.WriteHandle qh)
						return True
					Nothing -> return False
				let conflicted' = status == "U"
				procmergeconflictdiff mdfeeder rest
					(conflicted || conflicted')
			_ -> return conflicted -- parse failed
	procmergeconflictdiff _ _ conflicted = return conflicted

	reconcilepointerfile file key = do
		ics <- liftIO $ SQL.getInodeCaches key (SQL.ReadHandle qh)
		obj <- calcRepo (gitAnnexLocation key)
		mobjic <- withTSDelta (liftIO . genInodeCache obj)
		let addinodecaches k v = liftIO $
			SQL.addInodeCaches k v (SQL.WriteHandle qh)
		-- Like inAnnex, check the annex object is unmodified
		-- when annex.thin is set.
		keypopulated <- ifM (annexThin <$> Annex.getGitConfig)
			( case mobjic of
				Just objic -> isUnmodifiedLowLevel addinodecaches key obj objic ics
				Nothing -> pure False
			, pure (isJust mobjic)
			)
		p <- fromRepo $ fromTopFilePath file
		filepopulated <- sameInodeCache p ics
		case (keypopulated, filepopulated) of
			(True, False) ->
				populatePointerFile (Restage True) key obj p >>= \case
					Nothing -> return ()
					Just ic -> addinodecaches key
						(catMaybes [Just ic, mobjic])
			(False, True) -> depopulatePointerFile key p
			_ -> return ()
	
	send :: ((Maybe Key -> Annex a, Ref) -> IO ()) -> Ref -> (Maybe Key -> Annex a) -> IO ()
	send feeder r withk = feeder (withk, r)

	-- Streaming through git cat-file like this is significantly
	-- faster than using catKey.
	catstream a = do
		g <- Annex.gitRepo
		catObjectMetaDataStream g $ \mdfeeder mdcloser mdreader ->
			catObjectStream g $ \catfeeder catcloser catreader -> do
				feedt <- liftIO $ async $
					a mdfeeder
						`finally` void mdcloser
				proct <- liftIO $ async $
					procthread mdreader catfeeder
						`finally` void catcloser
				dbchanged <- dbwriter False largediff catreader
				-- Flush database changes now
				-- so other processes can see them.
				when dbchanged $
					liftIO $ H.flushDbQueue qh
				() <- liftIO $ wait feedt
				liftIO $ wait proct
				return ()
	  where
		procthread mdreader catfeeder = mdreader >>= \case
			Just (ka, Just (sha, size, _type))
				| size <= fromIntegral maxPointerSz -> do
					() <- catfeeder (ka, sha)
					procthread mdreader catfeeder
			Just _ -> procthread mdreader catfeeder
			Nothing -> return ()

		dbwriter dbchanged n catreader = liftIO catreader >>= \case
			Just (ka, content) -> do
				changed <- ka (parseLinkTargetOrPointerLazy =<< content)
				!n' <- countdownToMessage n
				dbwriter (dbchanged || changed) n' catreader
			Nothing -> return dbchanged

	-- When the diff is large, the scan can take a while,
	-- so let the user know what's going on.
	countdownToMessage n
		| n < 1 = return 0
		| n == 1 = do
			showSideAction "scanning for annexed files"
			return 0
		| otherwise = return (pred n)

	-- How large is large? Too large and there will be a long
	-- delay before the message is shown; too short and the message
	-- will clutter things up unncessarily. It's uncommon for 1000
	-- files to change in the index, and processing that many files
	-- takes less than half a second, so that seems about right.
	largediff :: Int
	largediff = 1000

	-- When the database is known to have been newly created and empty
	-- before reconcileStaged started, it is more efficient to use 
	-- newAssociatedFile. It's safe to use it here because this is run
	-- with a lock held that blocks any other process that opens the
	-- database, and when the database is newly created, there is no
	-- existing process that has it open already. And it's not possible
	-- for reconcileStaged to call this twice on the same filename with
	-- two different keys.
	addassociatedfile
		| dbisnew = SQL.newAssociatedFile
		| otherwise = SQL.addAssociatedFile

{- Normally the keys database is updated incrementally when opened,
 - by reconcileStaged. Calling this explicitly allows running the
 - update at an earlier point.
 -}
updateDatabase :: Annex ()
updateDatabase = runWriter ContentTable (const noop)
