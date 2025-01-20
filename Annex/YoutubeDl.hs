{- yt-dlp (and deprecated youtube-dl) integration for git-annex
 -
 - Copyright 2017-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE DeriveGeneric #-}

module Annex.YoutubeDl (
	youtubeDl,
	youtubeDlTo,
	youtubeDlSupported,
	youtubeDlCheck,
	youtubeDlFileName,
	youtubeDlFileNameHtmlOnly,
	youtubeDlCommand,
	youtubePlaylist,
	YoutubePlaylistItem(..),
) where

import Annex.Common
import qualified Annex
import Annex.Content
import Annex.Url
import Utility.DiskFree
import Utility.HtmlDetect
import Utility.Process.Transcript
import Utility.Metered
import Utility.Tmp
import Messages.Progress
import Logs.Transfer
import qualified Utility.RawFilePath as R

import Network.URI
import Control.Concurrent.Async
import Text.Read
import Data.Either
import qualified Data.Aeson as Aeson
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- youtube-dl can follow redirects to anywhere, including potentially
-- localhost or a private address. So, it's only allowed to download
-- content if the user has allowed access to all addresses.
youtubeDlAllowed :: Annex Bool
youtubeDlAllowed = ipAddressesUnlimited

youtubeDlNotAllowedMessage :: String
youtubeDlNotAllowedMessage = unwords
	[ "This url is supported by yt-dlp, but"
	, "yt-dlp could potentially access any address, and the"
	, "configuration of annex.security.allowed-ip-addresses"
	, "does not allow that. Not using yt-dlp (or youtube-dl)."
	]

-- Runs youtube-dl in a work directory, to download a single media file
-- from the url. Returns the path to the media file in the work directory.
--
-- Displays a progress meter as youtube-dl downloads.
--
-- If no file is downloaded, or the program is not installed,
-- returns Right Nothing.
--
-- youtube-dl can write to multiple files, either temporary files, or
-- multiple videos found at the url, and git-annex needs only one file.
-- So we need to find the destination file, and make sure there is not
-- more than one. With yt-dlp use --print-to-file to make it record the 
-- file(s) it downloads. With youtube-dl, the best that can be done is
-- to require that the work directory end up with only 1 file in it.
-- (This can fail, but youtube-dl is deprecated, and they closed my
-- issue requesting something like --print-to-file; 
-- <https://github.com/rg3/youtube-dl/issues/14864>)
youtubeDl :: URLString -> FilePath -> MeterUpdate -> Annex (Either String (Maybe FilePath))
youtubeDl url workdir p = ifM ipAddressesUnlimited
	( withUrlOptions $ youtubeDl' url workdir p
	, return $ Left youtubeDlNotAllowedMessage
	)

youtubeDl' :: URLString -> FilePath -> MeterUpdate -> UrlOptions -> Annex (Either String (Maybe FilePath))
youtubeDl' url workdir p uo
	| supportedScheme uo url = do
		cmd <- youtubeDlCommand
		ifM (liftIO $ inSearchPath cmd)
			( runcmd cmd >>= \case
				Right True -> downloadedfiles cmd >>= \case
					(f:[]) -> return (Right (Just f))
					[] -> return (nofiles cmd)
					fs -> return (toomanyfiles cmd fs)
				Right False -> workdirfiles >>= \case
					[] -> return (Right Nothing)
					_ -> return (Left $ cmd ++ " download is incomplete. Run the command again to resume.")
				Left msg -> return (Left msg)
			, return (Right Nothing)
			)
	| otherwise = return (Right Nothing)
  where
	nofiles cmd = Left $ cmd ++ " did not put any media in its work directory, perhaps it's been configured to store files somewhere else?"
	toomanyfiles cmd fs = Left $ cmd ++ " downloaded multiple media files; git-annex is only able to deal with one per url: " ++ show fs
	downloadedfiles cmd
		| isytdlp cmd = liftIO $ 
			(nub . lines <$> readFile filelistfile)
				`catchIO` (pure . const [])
		| otherwise = map fromRawFilePath <$> workdirfiles
	workdirfiles = liftIO $ filter (/= toRawFilePath filelistfile) 
		<$> (filterM R.doesPathExist =<< dirContents (toRawFilePath workdir))
	filelistfile = workdir </> filelistfilebase
	filelistfilebase = "git-annex-file-list-file"
	isytdlp cmd = cmd == "yt-dlp"
	runcmd cmd = youtubeDlMaxSize workdir >>= \case
		Left msg -> return (Left msg)
		Right maxsize -> do
			opts <- youtubeDlOpts (dlopts cmd ++ maxsize)
			oh <- mkOutputHandlerQuiet
			-- The size is unknown to start. Once youtube-dl
			-- outputs some progress, the meter will be updated
			-- with the size, which is why it's important the
			-- meter is passed into commandMeter'
			let unknownsize = Nothing :: Maybe FileSize
			ok <- metered (Just p) unknownsize Nothing $ \meter meterupdate ->
				liftIO $ commandMeter'
					(if isytdlp cmd then parseYtdlpProgress else parseYoutubeDlProgress)
					oh (Just meter) meterupdate cmd opts
					(\pr -> pr { cwd = Just workdir })
			return (Right ok)
	dlopts cmd = 
		[ Param url
		-- To make it only download one file when given a
		-- page with a video and a playlist, download only the video.
		, Param "--no-playlist"
		-- And when given a page with only a playlist, download only
		-- the first video on the playlist. (Assumes the video is
		-- somewhat stable, but this is the only way to prevent
		-- it from downloading the whole playlist.)
		, Param "--playlist-items", Param "0"
		] ++
			if isytdlp cmd
				then
					-- Avoid warnings, which go to
					-- stderr and may mess up
					-- git-annex's display.
					[ Param "--no-warnings"
					, Param "--progress-template"
					, Param progressTemplate
					, Param "--print-to-file"
					, Param "after_move:filepath"
					, Param filelistfilebase
					]
				else []

-- To honor annex.diskreserve, ask youtube-dl to not download too
-- large a media file. Factors in other downloads that are in progress,
-- and any files in the workdir that it may have partially downloaded
-- before.
youtubeDlMaxSize :: FilePath -> Annex (Either String [CommandParam])
youtubeDlMaxSize workdir = ifM (Annex.getRead Annex.force)
	( return $ Right []
	, liftIO (getDiskFree workdir) >>= \case
		Just have -> do
			inprogress <- sizeOfDownloadsInProgress (const True)
			partial <- liftIO $ sum 
				<$> (mapM getFileSize =<< dirContents (toRawFilePath workdir))
			reserve <- annexDiskReserve <$> Annex.getGitConfig
			let maxsize = have - reserve - inprogress + partial
			if maxsize > 0
				then return $ Right
					[ Param "--max-filesize"
					, Param (show maxsize)
					]
				else return $ Left $
					needMoreDiskSpace $
						negate maxsize + 1024
		Nothing -> return $ Right []
	)

-- Download a media file to a destination, 
youtubeDlTo :: Key -> URLString -> FilePath -> MeterUpdate -> Annex Bool
youtubeDlTo key url dest p = do
	res <- withTmpWorkDir key $ \workdir ->
		youtubeDl url (fromRawFilePath workdir) p >>= \case
			Right (Just mediafile) -> do
				liftIO $ moveFile (toRawFilePath mediafile) (toRawFilePath dest)
				return (Just True)
			Right Nothing -> return (Just False)
			Left msg -> do
				warning (UnquotedString msg)
				return Nothing
	return (fromMaybe False res)

-- youtube-dl supports downloading urls that are not html pages,
-- but we don't want to use it for such urls, since they can be downloaded
-- without it. So, this first downloads part of the content and checks 
-- if it's a html page; only then is youtube-dl used.
htmlOnly :: URLString -> a -> Annex a -> Annex a
htmlOnly url fallback a = withUrlOptions $ \uo -> 
	liftIO (downloadPartial url uo htmlPrefixLength) >>= \case
		Just bs | isHtmlBs bs -> a
		_ -> return fallback

-- Check if youtube-dl supports downloading content from an url.
youtubeDlSupported :: URLString -> Annex Bool
youtubeDlSupported url = either (const False) id
	<$> withUrlOptions (youtubeDlCheck' url)

-- Check if youtube-dl can find media in an url.
--
-- While this does not download anything, it checks youtubeDlAllowed
-- for symmetry with youtubeDl; the check should not succeed if the
-- download won't succeed.
youtubeDlCheck :: URLString -> Annex (Either String Bool)
youtubeDlCheck url = ifM youtubeDlAllowed
	( withUrlOptions $ youtubeDlCheck' url
	, return $ Left youtubeDlNotAllowedMessage
	)

youtubeDlCheck' :: URLString -> UrlOptions -> Annex (Either String Bool)
youtubeDlCheck' url uo
	| supportedScheme uo url = catchMsgIO $ htmlOnly url False $ do
		opts <- youtubeDlOpts [ Param url, Param "--simulate" ]
		cmd <- youtubeDlCommand
		liftIO $ snd <$> processTranscript cmd (toCommand opts) Nothing
	| otherwise = return (Right False)

-- Ask youtube-dl for the filename of media in an url.
--
-- (This is not always identical to the filename it uses when downloading.)
youtubeDlFileName :: URLString -> Annex (Either String FilePath)
youtubeDlFileName url = withUrlOptions go
  where
	go uo
		| supportedScheme uo url = flip catchIO (pure . Left . show) $
			htmlOnly url nomedia (youtubeDlFileNameHtmlOnly' url uo)
		| otherwise = return nomedia
	nomedia = Left "no media in url"

-- Does not check if the url contains htmlOnly; use when that's already
-- been verified.
youtubeDlFileNameHtmlOnly :: URLString -> Annex (Either String FilePath)
youtubeDlFileNameHtmlOnly = withUrlOptions . youtubeDlFileNameHtmlOnly'

youtubeDlFileNameHtmlOnly' :: URLString -> UrlOptions -> Annex (Either String FilePath)
youtubeDlFileNameHtmlOnly' url uo
	| supportedScheme uo url = flip catchIO (pure . Left . show) go
	| otherwise = return nomedia
  where
	go = do
		-- Sometimes youtube-dl will fail with an ugly backtrace
		-- (eg, http://bugs.debian.org/874321)
		-- so catch stderr as well as stdout to avoid the user
		-- seeing it. --no-warnings avoids warning messages that
		-- are output to stdout.
		opts <- youtubeDlOpts
			[ Param url
			, Param "--get-filename"
			, Param "--no-warnings"
			, Param "--no-playlist"
			]
		cmd <- youtubeDlCommand
		let p = (proc cmd (toCommand opts))
			{ std_out = CreatePipe
			, std_err = CreatePipe
			}
		liftIO $ withCreateProcess p waitproc
	
	waitproc Nothing (Just o) (Just e) pid = do
		errt <- async $ discardstderr pid e
		output <- hGetContentsStrict o
		ok <- liftIO $ checkSuccessProcess pid
		wait errt
		return $ case (ok, lines output) of
			(True, (f:_)) | not (null f) -> Right f
			_ -> nomedia
	waitproc _ _ _ _ = error "internal"

	discardstderr pid e = hGetLineUntilExitOrEOF pid e >>= \case
		Nothing -> return ()
		Just _ -> discardstderr pid e

	nomedia = Left "no media in url"

youtubeDlOpts :: [CommandParam] -> Annex [CommandParam]
youtubeDlOpts addopts = do
	opts <- map Param . annexYoutubeDlOptions <$> Annex.getGitConfig
	return (opts ++ addopts)

youtubeDlCommand :: Annex String
youtubeDlCommand = annexYoutubeDlCommand <$> Annex.getGitConfig >>= \case
	Just c -> pure c
	Nothing -> ifM (liftIO $ inSearchPath "yt-dlp")
		( return "yt-dlp"
		, return "youtube-dl"
		)

supportedScheme :: UrlOptions -> URLString -> Bool
supportedScheme uo url = case parseURIRelaxed url of
	Nothing -> False
	Just u -> case uriScheme u of
		-- avoid ugly message from youtube-dl about not supporting file:
		"file:" -> False
		-- ftp indexes may look like html pages, and there's no point
		-- involving youtube-dl in a ftp download
		"ftp:" -> False
		_ -> allowedScheme uo u

progressTemplate :: String
progressTemplate = "ANNEX %(progress.downloaded_bytes)i %(progress.total_bytes_estimate)i %(progress.total_bytes)i ANNEX"

{- The progressTemplate makes output look like "ANNEX 10 100 NA ANNEX" or
 - "ANNEX 10 NA 100 ANNEX" depending on whether the total bytes are estimated
 - or known. That makes parsing much easier (and less fragile) than parsing
 - the usual progress output.
 -}
parseYtdlpProgress :: ProgressParser
parseYtdlpProgress = go [] . reverse . progresschunks
  where
	delim = '\r'

	progresschunks = splitc delim

	go remainder [] = (Nothing, Nothing, remainder)
	go remainder (x:xs) = case splitc ' ' x of
			("ANNEX":downloaded_bytes_s:total_bytes_estimate_s:total_bytes_s:"ANNEX":[]) ->
				case (readMaybe downloaded_bytes_s, readMaybe total_bytes_estimate_s, readMaybe total_bytes_s) of
					(Just downloaded_bytes, Nothing, Just total_bytes) ->
						( Just (BytesProcessed downloaded_bytes)
						, Just (TotalSize total_bytes)
						, remainder
						)
					(Just downloaded_bytes, Just total_bytes_estimate, _) ->
						( Just (BytesProcessed downloaded_bytes)
						, Just (TotalSize total_bytes_estimate)
						, remainder
						)
					_ -> go (remainder++x) xs
			_ -> go (remainder++x) xs

{- youtube-dl is deprecated, parsing its progress was attempted before but
 - was buggy and is no longer done. -}
parseYoutubeDlProgress :: ProgressParser
parseYoutubeDlProgress _ = (Nothing, Nothing, "")

{- List the items that yt-dlp can download from an url.
 - 
 - Note that this does not check youtubeDlAllowed because it does not
 - download content.
 -}
youtubePlaylist :: URLString -> Annex (Either String [YoutubePlaylistItem])
youtubePlaylist url = do
	cmd <- youtubeDlCommand
	if cmd == "yt-dlp"
		then liftIO $ youtubePlaylist' url cmd
		else return $ Left $ "Scraping needs yt-dlp, but git-annex has been configured to use " ++ cmd

youtubePlaylist' :: URLString -> String -> IO (Either String [YoutubePlaylistItem])
youtubePlaylist' url cmd = withTmpFile "yt-dlp" $ \tmpfile h -> do
	hClose h
	(outerr, ok) <- processTranscript cmd
		[ "--simulate"
		, "--flat-playlist"
		-- Skip live videos in progress
		, "--match-filter", "!is_live"
		, "--print-to-file"
		-- Write json with selected fields.
		, "%(.{" ++ intercalate "," youtubePlaylistItemFields ++ "})j"
		, tmpfile
		, url
		]
		Nothing
	if ok
		then flip catchIO (pure . Left . show) $ do
			v <- map Aeson.eitherDecodeStrict . B8.lines
				<$> B.readFile tmpfile
			return $ case partitionEithers v of
				((parserr:_), _) -> 
					Left $ "yt-dlp json parse error: " ++ parserr
				([], r) -> Right r
		else return $ Left $ if null outerr
			then "yt-dlp failed"
			else "yt-dlp failed: " ++ outerr

-- There are other fields that yt-dlp can extract, but these are similar to
-- the information from an RSS feed.
youtubePlaylistItemFields :: [String]
youtubePlaylistItemFields =
	[ "playlist_title"
	, "playlist_uploader"
	, "title"
	, "description"
	, "license"
	, "url"
	, "timestamp"
	]

-- Parse JSON generated by yt-dlp for playlist. Note that any field
-- may be omitted when that information is not supported for a given website.
data YoutubePlaylistItem = YoutubePlaylistItem
	{ youtube_playlist_title :: Maybe String
	, youtube_playlist_uploader :: Maybe String
	, youtube_title :: Maybe String
	, youtube_description :: Maybe String
	, youtube_license :: Maybe String
	, youtube_url :: Maybe String
	, youtube_timestamp :: Maybe Integer -- ^ unix timestamp
	} deriving (Generic, Show)

instance Aeson.FromJSON YoutubePlaylistItem
  where
	parseJSON = Aeson.genericParseJSON Aeson.defaultOptions
		{ Aeson.fieldLabelModifier = drop (length "youtube_") }

