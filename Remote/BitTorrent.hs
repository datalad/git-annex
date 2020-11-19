{- BitTorrent remote.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Remote.BitTorrent (remote) where

import Annex.Common
import Types.Remote
import qualified Annex
import qualified Git
import qualified Git.Construct
import Config
import Config.Cost
import Logs.Web
import Types.UrlContents
import Types.CleanupActions
import Messages.Progress
import Utility.Metered
import Utility.Tmp
import Backend.URL
import Annex.Perms
import Annex.Tmp
import Annex.UUID
import qualified Annex.Url as Url
import Remote.Helper.ExportImport
import Annex.SpecialRemote.Config
import qualified Utility.RawFilePath as R

import Network.URI
import qualified System.FilePath.ByteString as P

#ifdef WITH_TORRENTPARSER
import Data.Torrent
import qualified Data.ByteString.Lazy as B
#endif

remote :: RemoteType
remote = RemoteType
	{ typename = "bittorrent"
	, enumerate = list
	, generate = gen
	, configParser = mkRemoteConfigParser []
	, setup = error "not supported"
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

-- There is only one bittorrent remote, and it always exists.
list :: Bool -> Annex [Git.Repo]
list _autoinit = do
	r <- liftIO $ Git.Construct.remoteNamed "bittorrent" (pure Git.Construct.fromUnknown)
	return [r]

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r _ rc gc rs = do
	cst <- remoteCost gc expensiveRemoteCost
	c <- parsedRemoteConfig remote rc
	return $ Just Remote
		{ uuid = bitTorrentUUID
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = uploadKey
		, retrieveKeyFile = downloadKey
		, retrieveKeyFileCheap = Nothing
		-- Bittorrent does its own hash checks.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = dropKey
		, lockContent = Nothing
		, checkPresent = checkKey
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, gitconfig = gc
		, localpath = Nothing
		, getRepo = return r
		, readonly = True
		, appendonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = return []
		, claimUrl = Just (pure . isSupportedUrl)
		, checkUrl = Just checkTorrentUrl
		, remoteStateHandle = rs
		}

downloadKey :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
downloadKey key _file dest p = do
	get . map (torrentUrlNum . fst . getDownloader) =<< getBitTorrentUrls key
	return UnVerified
  where
	get [] = giveup "could not download torrent"
	get urls = do
		showOutput -- make way for download progress bar
		ok <- untilTrue urls $ \(u, filenum) -> do
			registerTorrentCleanup u
			checkDependencies
			ifM (downloadTorrentFile u)
				( downloadTorrentContent key u dest filenum p
				, return False
				)
		unless ok $
			get []

uploadKey :: Key -> AssociatedFile -> MeterUpdate -> Annex ()
uploadKey _ _ _ = giveup "upload to bittorrent not supported"

dropKey :: Key -> Annex ()
dropKey k = mapM_ (setUrlMissing k) =<< getBitTorrentUrls k

{- We punt and don't try to check if a torrent has enough seeders
 - with all the pieces etc. That would be quite hard.. and even if
 - implemented, it tells us nothing about the later state of the torrent.
 -}
checkKey :: Key -> Annex Bool
checkKey = giveup "cannot reliably check torrent status"

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
		, giveup "could not download torrent file"
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

{- Temporary filename to use to store the torrent file. -}
tmpTorrentFile :: URLString -> Annex RawFilePath
tmpTorrentFile u = fromRepo . gitAnnexTmpObjectLocation =<< torrentUrlKey u

{- A cleanup action is registered to delete the torrent file
 - when git-annex exits.
 -
 - This allows multiple actions that use the same torrent file
 - directory to run in a single git-annex run, and only download the
 - torrent file once.
 -}
registerTorrentCleanup :: URLString -> Annex ()
registerTorrentCleanup u = Annex.addCleanup (TorrentCleanup u) $
	liftIO . removeWhenExistsWith R.removeLink =<< tmpTorrentFile u

{- Downloads the torrent file. (Not its contents.) -}
downloadTorrentFile :: URLString -> Annex Bool
downloadTorrentFile u = do
	torrent <- tmpTorrentFile u
	ifM (liftIO $ doesFileExist (fromRawFilePath torrent))
		( return True
		, do
			showAction "downloading torrent file"
			createAnnexDirectory (parentDir torrent)
			if isTorrentMagnetUrl u
				then withOtherTmp $ \othertmp -> do
					kf <- keyFile <$> torrentUrlKey u
					let metadir = othertmp P.</> "torrentmeta" P.</> kf
					createAnnexDirectory metadir
					showOutput
					ok <- downloadMagnetLink u
						(fromRawFilePath metadir)
						(fromRawFilePath torrent)
					liftIO $ removeDirectoryRecursive
						(fromRawFilePath metadir)
					return ok
				else withOtherTmp $ \othertmp -> do
					withTmpFileIn (fromRawFilePath othertmp) "torrent" $ \f h -> do
						liftIO $ hClose h
						resetAnnexFilePerm (toRawFilePath f)
						ok <- Url.withUrlOptions $ 
							Url.download nullMeterUpdate u f
						when ok $
							liftIO $ renameFile f (fromRawFilePath torrent)
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
	withOtherTmp $ \othertmp -> do
		kf <- keyFile <$> torrentUrlKey u
		let downloaddir = othertmp P.</> "torrent" P.</> kf
		createAnnexDirectory downloaddir
		f <- wantedfile torrent
		let dlf = fromRawFilePath downloaddir </> f
		showOutput
		ifM (download torrent downloaddir <&&> liftIO (doesFileExist dlf))
			( do
				liftIO $ renameFile dlf dest
				-- The downloaddir is not removed here,
				-- so if aria downloaded parts of other
				-- files, and this is called again, it will
				-- resume where it left off.
				-- withOtherTmp registers a cleanup action
				-- that will clean up leftover files when
				-- git-annex terminates.
				return True
			, return False
			)
  where
	download torrent tmpdir = ariaProgress (fromKey keySize k) p
		[ Param $ "--select-file=" ++ show filenum
		, File (fromRawFilePath torrent)
		, Param "-d"
		, File (fromRawFilePath tmpdir)
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
			else giveup "Number of files in torrent seems to have changed."

checkDependencies :: Annex ()
checkDependencies = do
	missing <- liftIO $ filterM (not <$$> inPath) deps
	unless (null missing) $
		giveup $ "need to install additional software in order to download from bittorrent: " ++ unwords missing
  where
	deps =
		[ "aria2c"
#ifndef WITH_TORRENTPARSER
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
	liftIO . commandMeter (parseAriaProgress sz) oh Nothing meter "aria2c"
		=<< ariaParams ps

parseAriaProgress :: Integer -> ProgressParser
parseAriaProgress totalsize = go [] . reverse . splitc '\r'
  where
	go remainder [] = (Nothing, Nothing, remainder)
	go remainder (x:xs) = case readish (findpercent x) of
		Nothing -> go (x++remainder) xs
		Just p -> (Just (frompercent p), Nothing, remainder)

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
torrentFileSizes :: RawFilePath -> IO [(FilePath, Integer)]
torrentFileSizes torrent = do
#ifdef WITH_TORRENTPARSER
	let mkfile = joinPath . map (scrub . decodeBL)
	b <- B.readFile (fromRawFilePath torrent)
	return $ case readTorrent b of
		Left e -> giveup $ "failed to parse torrent: " ++ e
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
	getfield = btshowmetainfo (fromRawFilePath torrent)
	parsefailed s = giveup $ "failed to parse btshowmetainfo output for torrent file: " ++ show s

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
		then giveup "found unsafe filename in torrent!"
		else f

torrentContents :: URLString -> Annex UrlContents
torrentContents u = convert
	<$> (liftIO . torrentFileSizes =<< tmpTorrentFile u)
  where
	convert [(fn, sz)] = UrlContents (Just sz) (Just fn)
	convert l = UrlMulti $ map mkmulti (zip l [1..])

	mkmulti ((fn, sz), n) = 
		(torrentUrlWithNum u n, Just sz, joinPath $ drop 1 $ splitPath fn)
