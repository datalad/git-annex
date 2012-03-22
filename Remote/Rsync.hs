{- A remote that is only accessible by rsync.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Rsync (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.RsyncFile

type RsyncUrl = String

data RsyncOpts = RsyncOpts {
	rsyncUrl :: RsyncUrl,
	rsyncOptions :: [CommandParam]
}

remote :: RemoteType
remote = RemoteType {
	typename = "rsync",
	enumerate = findSpecialRemotes "rsyncurl",
	generate = gen,
	setup = rsyncSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	o <- genRsyncOpts r
	cst <- remoteCost r expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted o)
		(retrieveEncrypted o)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store o,
			retrieveKeyFile = retrieve o,
			retrieveKeyFileCheap = retrieveCheap o,
			removeKey = remove o,
			hasKey = checkPresent r o,
			hasKeyCheap = False,
			whereisKey = Nothing,
			config = Nothing,
			repo = r,
			remotetype = remote
		}

genRsyncOpts :: Git.Repo -> Annex RsyncOpts
genRsyncOpts r = do
	url <- getRemoteConfig r "rsyncurl" (error "missing rsyncurl")
	opts <- getRemoteConfig r "rsync-options" ""
	return $ RsyncOpts url $ map Param $ filter safe $ words opts
	where
		safe o
			-- Don't allow user to pass --delete to rsync;
			-- that could cause it to delete other keys
			-- in the same hash bucket as a key it sends.
			| o == "--delete" = False
			| o == "--delete-excluded" = False
			| otherwise = True

rsyncSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
rsyncSetup u c = do
	-- verify configuration is sane
	let url = fromMaybe (error "Specify rsyncurl=") $
		M.lookup "rsyncurl" c
	c' <- encryptionSetup c

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "rsyncurl" url
	return c'

rsyncEscape :: RsyncOpts -> String -> String
rsyncEscape o s
	| rsyncUrlIsShell (rsyncUrl o) = shellEscape s
	| otherwise = s

rsyncUrls :: RsyncOpts -> Key -> [String]
rsyncUrls o k = map use annexHashes
	where
		use h = rsyncUrl o </> h k </> rsyncEscape o (f </> f)
                f = keyFile k

store :: RsyncOpts -> Key -> Annex Bool
store o k = rsyncSend o k =<< inRepo (gitAnnexLocation k)

storeEncrypted :: RsyncOpts -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted o (cipher, enck) k = withTmp enck $ \tmp -> do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ withEncryptedContent cipher (L.readFile src) $ L.writeFile tmp
	rsyncSend o enck tmp

retrieve :: RsyncOpts -> Key -> FilePath -> Annex Bool
retrieve o k f = untilTrue (rsyncUrls o k) $ \u -> rsyncRemote o
	-- use inplace when retrieving to support resuming
	[ Param "--inplace"
	, Param u
	, Param f
	]

retrieveCheap :: RsyncOpts -> Key -> FilePath -> Annex Bool
retrieveCheap o k f = ifM (preseedTmp k f) ( retrieve o k f , return False )

retrieveEncrypted :: RsyncOpts -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted o (cipher, enck) _ f = withTmp enck $ \tmp -> do
	ifM (retrieve o enck tmp)
		( liftIO $ catchBoolIO $ do
			withDecryptedContent cipher (L.readFile tmp) $ L.writeFile f
			return True
		, return False
		)

remove :: RsyncOpts -> Key -> Annex Bool
remove o k = withRsyncScratchDir $ \tmp -> liftIO $ do
	{- Send an empty directory to rysnc to make it delete. -}
	let dummy = tmp </> keyFile k
	createDirectoryIfMissing True dummy
	rsync $ rsyncOptions o ++
		map (\s -> Param $ "--include=" ++ s) includes ++
		[ Param "--exclude=*" -- exclude everything else
		, Params "--quiet --delete --recursive"
		, partialParams
		, Param $ addTrailingPathSeparator dummy
		, Param $ rsyncUrl o
		]
	where
		{- Specify include rules to match the directories where the
		 - content could be. Note that the parent directories have
		 - to also be explicitly included, due to how rsync
		 - traverses directories. -}
		includes = concatMap use annexHashes
		use h = let dir = h k in
			[ parentDir dir
			, dir
			-- match content directory and anything in it
			, dir </> keyFile k </> "***"
			]

checkPresent :: Git.Repo -> RsyncOpts -> Key -> Annex (Either String Bool)
checkPresent r o k = do
	showAction $ "checking " ++ Git.repoDescribe r
	-- note: Does not currently differentiate between rsync failing
	-- to connect, and the file not being present.
	Right <$> check
	where
 		check = untilTrue (rsyncUrls o k) $ \u ->
			liftIO $ boolSystem "sh" [Param "-c", Param (cmd u)]
		cmd u = "rsync --quiet " ++ shellEscape u ++ " 2>/dev/null"

{- Rsync params to enable resumes of sending files safely,
 - ensure that files are only moved into place once complete
 -}
partialParams :: CommandParam
partialParams = Params "--no-inplace --partial --partial-dir=.rsync-partial"

{- Runs an action in an empty scratch directory that can be used to build
 - up trees for rsync. -}
withRsyncScratchDir :: (FilePath -> Annex Bool) -> Annex Bool
withRsyncScratchDir a = do
	pid <- liftIO getProcessID
	t <- fromRepo gitAnnexTmpDir
	let tmp = t </> "rsynctmp" </> show pid
	nuke tmp
	liftIO $ createDirectoryIfMissing True tmp
	nuke tmp `after` a tmp
	where
		nuke d = liftIO $ whenM (doesDirectoryExist d) $
			removeDirectoryRecursive d

rsyncRemote :: RsyncOpts -> [CommandParam] -> Annex Bool
rsyncRemote o params = do
	showOutput -- make way for progress bar
	ifM (liftIO $ rsync $ rsyncOptions o ++ defaultParams ++ params)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)
	where
		defaultParams = [Params "--progress"]

{- To send a single key is slightly tricky; need to build up a temporary
   directory structure to pass to rsync so it can create the hash
   directories. -}
rsyncSend :: RsyncOpts -> Key -> FilePath -> Annex Bool
rsyncSend o k src = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> Prelude.head (keyPaths k)
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	liftIO $ createLink src dest
	rsyncRemote o
		[ Param "--recursive"
		, partialParams
 		  -- tmp/ to send contents of tmp dir
		, Param $ addTrailingPathSeparator tmp
		, Param $ rsyncUrl o
		]
