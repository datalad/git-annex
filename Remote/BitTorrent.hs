{- BitTorrent remote.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.BitTorrent (remote) where

import Common.Annex
import Types.Remote
import qualified Annex
import qualified Git
import qualified Git.Construct
import Config.Cost
import Logs.Web
import Types.UrlContents
import Types.CleanupActions
import Types.Key
import Messages.Progress
import Utility.Metered
import Utility.Tmp
import Backend.URL
import Annex.Perms
import Annex.UUID
import qualified Annex.Url as Url

import Network.URI

#ifdef WITH_TORRENTPARSER
import Data.Torrent
import qualified Data.ByteString.Lazy as B
#endif

remote :: RemoteType
remote = RemoteType {
	typename = "bittorrent",
	enumerate = list,
	generate = gen,
	setup = error "not supported"
}

-- There is only one bittorrent remote, and it always exists.
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "bittorrent" (pure Git.Construct.fromUnknown)
	return [r]

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r _ c gc = 
	return $ Just Remote
		{ uuid = bitTorrentUUID
		, cost = expensiveRemoteCost
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey
		, retrieveKeyFileCheap = downloadKeyCheap
		, removeKey = dropKey
		, lockContent = Nothing
		, checkPresent = checkKey
		, checkPresentCheap = False
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, repo = r
		, readonly = True
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Just (pure . isSupportedUrl)
		, checkUrl = Just checkTorrentUrl
		}

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
downloadKey key _file dest p = unVerified $
	get . map (torrentUrlNum . fst . getDownloader) =<< getBitTorrentUrls key
  where
	get [] = do
		warning "could not download torrent"
		return False
	get urls = do
		showOutput -- make way for download progress bar
		untilTrue urls $ \(u, filenum) -> do
			registerTorrentCleanup u
			checkDependencies
			ifM (downloadTorrentFile u)
				( downloadTorrentContent key u dest filenum p
				, return False
				)

downloadKeyCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
downloadKeyCheap _ _ _ = return False

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
uploadKey _ _ _ = do
	warning "upload to bittorrent not supported"
	return False

dropKey :: Key -> Annex Bool
dropKey k = do
	mapM_ (setUrlMissing bitTorrentUUID k) =<< getBitTorrentUrls k
	return True

{- We punt and don't try to check if a torrent has enough seeders
 - with all the pieces etc. That would be quite hard.. and even if
 - implemented, it tells us nothing about the later state of the torrent.
 -}
checkKey :: Key -> Annex Bool
checkKey = error "cannot reliably check torrent status"

getBitTorrentUrls :: Key -> Annex [URLString]
getBitTorrentUrls key = filter supported <$> getUrls key
  where
	supported u =
		let (u', dl) = (getDownloader u)
		in dl == OtherDownloader && isSupportedUrl u'

isSupportedUrl :: URLString -> Bool
isSupportedUrl u = isTorrentMagnetUrl u || isTorrentUrl u

isTorrentUrl :: URLString -> Bool
isTorrentUrl = maybe False (\u -> ".torrent" `isSuffixOf` uriPath u) . parseURI

isTorrentMagnetUrl :: URLString -> Bool
isTorrentMagnetUrl u = "magnet:" `isPrefixOf` u && checkbt (parseURI u)
  where
	checkbt (Just uri) | "xt=urn:btih:" `isInfixOf` uriQuery uri = True
	checkbt _ = False

checkTorrentUrl :: URLString -> Annex UrlContents
checkTorrentUrl u = do
	checkDependencies
	registerTorrentCleanup u
	ifM (downloadTorrentFile u)
		( torrentContents u
		, error "could not download torrent file"
		)

{- To specify which file inside a multi-url torrent, the file number is
 - appended to the url. -}
torrentUrlWithNum :: URLString -> Int -> URLString
torrentUrlWithNum u n = u ++ "#" ++ show n

torrentUrlNum :: URLString -> (URLString, Int)
torrentUrlNum u
	| '#' `elem` u = 
		let (n, ru) = separate (== '#') (reverse u)
		in (reverse ru, fromMaybe 1 $ readish $ reverse n)
	| otherwise = (u, 1)

{- A Key corresponding to the URL of a torrent file. -}
torrentUrlKey :: URLString -> Annex Key
torrentUrlKey u = return $ fromUrl (fst $ torrentUrlNum u) Nothing

{- Temporary directory used to download a torrent. -}
tmpTorrentDir :: URLString -> Annex FilePath
tmpTorrentDir u = do
	d <- fromRepo gitAnnexTmpMiscDir
	f <- keyFile <$> torrentUrlKey u
	return (d </> f)

{- Temporary filename to use to store the torrent file. -}
tmpTorrentFile :: URLString -> Annex FilePath
tmpTorrentFile u = fromRepo . gitAnnexTmpObjectLocation =<< torrentUrlKey u

{- A cleanup action is registered to delete the torrent file and its
 - associated temp directory when git-annex exits.
 -
 - This allows multiple actions that use the same torrent file and temp
 - directory to run in a single git-annex run.
 -}
registerTorrentCleanup :: URLString -> Annex ()
registerTorrentCleanup u = Annex.addCleanup (TorrentCleanup u) $ do
	liftIO . nukeFile =<< tmpTorrentFile u
	d <- tmpTorrentDir u
	liftIO $ whenM (doesDirectoryExist d) $
		removeDirectoryRecursive d

{- Downloads the torrent file. (Not its contents.) -}
downloadTorrentFile :: URLString -> Annex Bool
downloadTorrentFile u = do
	torrent <- tmpTorrentFile u
	ifM (liftIO $ doesFileExist torrent)
		( return True
		, do
			showAction "downloading torrent file"
			showOutput
			createAnnexDirectory (parentDir torrent)
			if isTorrentMagnetUrl u
				then do
					tmpdir <- tmpTorrentDir u
					let metadir = tmpdir </> "meta"
					createAnnexDirectory metadir
					ok <- downloadMagnetLink u metadir torrent
					liftIO $ removeDirectoryRecursive metadir
					return ok
				else do
					misctmp <- fromRepo gitAnnexTmpMiscDir
					withTmpFileIn misctmp "torrent" $ \f h -> do
						liftIO $ hClose h
						ok <- Url.withUrlOptions $ Url.download u f
						when ok $
							liftIO $ renameFile f torrent
						return ok
		)

downloadMagnetLink :: URLString -> FilePath -> FilePath -> Annex Bool
downloadMagnetLink u metadir dest = ifM download
	( liftIO $ do
		ts <- filter (".torrent" `isSuffixOf`)
			<$> dirContents metadir
		case ts of
			(t:[]) -> do
				renameFile t dest
				return True
			_ -> return False
	, return False
	)
  where
	download = runAria
		[ Param "--bt-metadata-only"
		, Param "--bt-save-metadata"
		, Param u
		, Param "--seed-time=0"
		, Param "--summary-interval=0"
		, Param "-d"
		, File metadir
		]

downloadTorrentContent :: Key -> URLString -> FilePath -> Int -> MeterUpdate -> Annex Bool
downloadTorrentContent k u dest filenum p = do
	torrent <- tmpTorrentFile u
	tmpdir <- tmpTorrentDir u
	createAnnexDirectory tmpdir
	f <- wantedfile torrent
	showOutput
	ifM (download torrent tmpdir <&&> liftIO (doesFileExist (tmpdir </> f)))
		( do
			liftIO $ renameFile (tmpdir </> f) dest
			return True
		, return False
		)
  where
	download torrent tmpdir = ariaProgress (keySize k) p
		[ Param $ "--select-file=" ++ show filenum
		, File torrent
		, Param "-d"
		, File tmpdir
		, Param "--seed-time=0"
		, Param "--summary-interval=0"
		, Param "--file-allocation=none"
		-- Needed so aria will resume partially downloaded files
		-- in multi-file torrents.
		, Param "--check-integrity=true"
		]
	
	{- aria2c will create part of the directory structure
	 - contained in the torrent. It may download parts of other files
	 - in addition to the one we asked for. So, we need to find
	 - out the filename we want based on the filenum.
	 -}
	wantedfile torrent = do
		fs <- liftIO $ map fst <$> torrentFileSizes torrent
		if length fs >= filenum
			then return (fs !! (filenum - 1))
			else error "Number of files in torrent seems to have changed."

checkDependencies :: Annex ()
checkDependencies = do
	missing <- liftIO $ filterM (not <$$> inPath) deps
	unless (null missing) $
		error $ "need to install additional software in order to download from bittorrent: " ++ unwords missing
  where
	deps =
		[ "aria2c"
#ifndef TORRENT
		, "btshowmetainfo"
#endif
		]

ariaParams :: [CommandParam] -> Annex [CommandParam]
ariaParams ps = do
	opts <- map Param . annexAriaTorrentOptions <$> Annex.getGitConfig
	return (ps ++ opts)

runAria :: [CommandParam] -> Annex Bool
runAria ps = progressCommand "aria2c" =<< ariaParams ps

-- Parse aria output to find "(n%)" and update the progress meter
-- with it.
ariaProgress :: Maybe Integer -> MeterUpdate -> [CommandParam] -> Annex Bool
ariaProgress Nothing _ ps = runAria ps
ariaProgress (Just sz) meter ps = do
	oh <- mkOutputHandler
	liftIO . commandMeter (parseAriaProgress sz) oh meter "aria2c"
		=<< ariaParams ps

parseAriaProgress :: Integer -> ProgressParser
parseAriaProgress totalsize = go [] . reverse . split ['\r']
  where
	go remainder [] = (Nothing, remainder)
	go remainder (x:xs) = case readish (findpercent x) of
		Nothing -> go (x++remainder) xs
		Just p -> (Just (frompercent p), remainder)

	-- "(N%)"
	findpercent = takeWhile (/= '%') . drop 1 . dropWhile (/= '(')

	frompercent p = toBytesProcessed $ totalsize * p `div` 100

{- Used only if the haskell torrent library is not available. -}
#ifndef WITH_TORRENTPARSER
btshowmetainfo :: FilePath -> String -> IO [String]
btshowmetainfo torrent field = 
	findfield [] . lines <$> readProcess "btshowmetainfo" [torrent]
  where
	findfield c [] = reverse c
	findfield c (l:ls)
		| l == fieldkey = multiline c ls
		| fieldkey `isPrefixOf` l =
			findfield ((drop (length fieldkey) l):c) ls
		| otherwise = findfield c ls

	multiline c (l:ls)
		| "   " `isPrefixOf` l = multiline (drop 3 l:c) ls
		| otherwise = findfield c ls
	multiline c [] = findfield c []

	fieldkey = field ++ take (14 - length field) (repeat '.') ++ ": "
#endif

{- Examines the torrent file and gets the list of files in it,
 - and their sizes.
 -}
torrentFileSizes :: FilePath -> IO [(FilePath, Integer)]
torrentFileSizes torrent = do
#ifdef WITH_TORRENTPARSER
	let mkfile = joinPath . map (scrub . decodeBS)
	b <- B.readFile torrent
	return $ case readTorrent b of
		Left e -> error $ "failed to parse torrent: " ++ e
		Right t -> case tInfo t of
			SingleFile { tLength = l, tName = f } ->
				[ (mkfile [f], l) ]
			MultiFile { tFiles = fs, tName = dir } ->
				map (\tf -> (mkfile $ dir:filePath tf, fileLength tf)) fs
  where
#else
	files <- getfield "files"
	if null files
		then do
			fnl <- getfield "file name"
			szl <- map readish <$> getfield "file size"
			case (fnl, szl) of
				((fn:[]), (Just sz:[])) -> return [(scrub fn, sz)]
		 		_ -> parsefailed (show (fnl, szl))
		else do
			v <- getfield "directory name"
			case v of
				(d:[]) -> return $ map (splitsize d) files
				_ -> parsefailed (show v)
  where
	getfield = btshowmetainfo torrent
	parsefailed s = error $ "failed to parse btshowmetainfo output for torrent file: " ++ show s

	-- btshowmetainfo outputs a list of "filename (size)"
	splitsize d l = (scrub (d </> fn), sz)
	  where
		sz = fromMaybe (parsefailed l) $ readish $ 
			reverse $ takeWhile (/= '(') $ dropWhile (== ')') $
				reverse l
		fn = reverse $ drop 2 $
			dropWhile (/= '(') $ dropWhile (== ')') $ reverse l
#endif
	-- a malicious torrent file might try to do directory traversal
	scrub f = if isAbsolute f || any (== "..") (splitPath f)
		then error "found unsafe filename in torrent!"
		else f

torrentContents :: URLString -> Annex UrlContents
torrentContents u = convert
	<$> (liftIO . torrentFileSizes =<< tmpTorrentFile u)
  where
	convert [(fn, sz)] = UrlContents (Just sz) (Just (mkSafeFilePath fn))
	convert l = UrlMulti $ map mkmulti (zip l [1..])

	mkmulti ((fn, sz), n) = 
		(torrentUrlWithNum u n, Just sz, mkSafeFilePath $ joinPath $ drop 1 $ splitPath fn)
