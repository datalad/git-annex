{- git repository recovery
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.RecoverRepository (
	cleanCorruptObjects,
	retrieveMissingObjects,
	resetLocalBranches,
	removeTrackingBranches,
) where

import Common
import Git
import Git.Command
import Git.Fsck
import Git.Objects
import Git.HashObject
import Git.Types
import qualified Git.Config
import qualified Git.Construct
import Utility.Tmp
import Utility.Monad
import Utility.Rsync

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import System.Log.Logger

{- Finds and removes corrupt objects from the repository, returning a list
 - of all such objects, which need to be found elsewhere to finish
 - recovery.
 -
 - Strategy: Run git fsck, remove objects it identifies as corrupt,
 - and repeat until git fsck finds no new objects.
 -
 - To remove corrupt objects, unpack all packs, and remove the packs
 - (to handle corrupt packs), and remove loose object files.
 -}
cleanCorruptObjects :: Repo -> IO (S.Set Sha)
cleanCorruptObjects r = do
	notice "Running git fsck ..."
	check =<< findBroken r
  where
  	check Nothing = do
		notice "git fsck found a problem but no specific broken objects. Perhaps a corrupt pack file? Unpacking all pack files."
		explodePacks r
		retry S.empty
	check (Just bad)
		| S.null bad = return S.empty
		| otherwise = do
			notice $ unwords 
				[ "git fsck found"
				, show (S.size bad)
				, "broken objects. Unpacking all pack files."
				]
			explodePacks r
			removeLoose r bad
			retry bad
	retry oldbad = do
		notice "Re-running git fsck to see if it finds more problems."
		v <- findBroken r
		case v of
			Nothing -> error $ unwords
				[ "git fsck found a problem, which was not corrected after removing"
				, show (S.size oldbad)
				, "corrupt objects."
				]
			Just newbad -> do
				removeLoose r newbad
				let s = S.union oldbad newbad
				if s == oldbad
					then return s
					else retry s

removeLoose :: Repo -> S.Set Sha -> IO ()
removeLoose r s = do
	let fs = map (looseObjectFile r) (S.toList s)
	count <- length <$> filterM doesFileExist fs
	when (count > 0) $ do
		notice $ unwords
			[ "removing"
			, show count
			, "corrupt loose objects"
			]
		mapM_ nukeFile fs

explodePacks :: Repo -> IO ()
explodePacks r = mapM_ go =<< listPackFiles r
  where
	go packfile = do
		-- May fail, if pack file is corrupt.
		void $ tryIO $
			pipeWrite [Param "unpack-objects"] r $ \h ->
				L.hPut h =<< L.readFile packfile
		nukeFile packfile
		nukeFile $ packIdxFile packfile

{- Try to retrieve a set of missing objects, from the remotes of a
 - repository. Returns any that could not be retreived.
 -}
retrieveMissingObjects :: S.Set Sha -> Repo -> IO (S.Set Sha)
retrieveMissingObjects missing r
	| S.null missing = return missing
	| otherwise = withTmpDir "tmprepo" $ \tmpdir -> do
		unlessM (boolSystem "git" [Params "init", File tmpdir]) $
			error $ "failed to create temp repository in " ++ tmpdir
		tmpr <- Git.Config.read =<< Git.Construct.fromAbsPath tmpdir
		stillmissing <- pullremotes tmpr (remotes r) fetchrefstags missing
		if S.null stillmissing
			then return stillmissing
			else pullremotes tmpr (remotes r) fetchallrefs stillmissing
  where
	pullremotes tmpr [] _ stillmissing = return stillmissing
	pullremotes tmpr (rmt:rmts) fetchrefs s
		| S.null s = return s
		| otherwise = do
			notice $ "Trying to recover missing objects from remote " ++ repoDescribe rmt
			ifM (fetchsome rmt fetchrefs tmpr)
				( do
					void $ copyObjects tmpr r
					stillmissing <- findMissing (S.toList s) r
					pullremotes tmpr rmts fetchrefs stillmissing
				, do
					notice $ unwords
						[ "failed to fetch from remote"
						, repoDescribe rmt
						, "(will continue without it, but making this remote available may improve recovery)"
						]
					pullremotes tmpr rmts fetchrefs s
				)
	fetchsome rmt ps = runBool $
		[ Param "fetch"
		, Param (repoLocation rmt)
		, Params "--force --update-head-ok --quiet"
		] ++ ps
	-- fetch refs and tags
	fetchrefstags = [ Param "+refs/heads/*:refs/heads/*", Param "--tags"]
	-- Fetch all available refs (more likely to fail,
	-- as the remote may have refs it refuses to send).
	fetchallrefs = [ Param "+*:*" ]

{- Copies all objects from the src repository to the dest repository.
 - This is done using rsync, so it copies all missing object, and all
 - objects they rely on. -}
copyObjects :: Repo -> Repo -> IO Bool
copyObjects srcr destr = rsync
	[ Param "-qr"
	, File $ addTrailingPathSeparator $ objectsDir srcr
	, File $ addTrailingPathSeparator $ objectsDir destr
	]

{- To deal with missing objects that cannot be recovered, resets any
 - local branches to point to an old commit before the missing
 - objects.
 -}
resetLocalBranches :: S.Set Sha -> Repo -> IO [Branch]
resetLocalBranches missing r = do
	error "TODO"

{- To deal with missing objects that cannot be recovered, removes
 - any remote tracking branches that reference them.
 -}
removeTrackingBranches :: S.Set Sha -> Repo -> IO [Branch]
removeTrackingBranches missing r = do
	error "TODO"

notice :: String -> IO ()
notice = noticeM "RecoverRepository"
