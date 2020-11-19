{- youtube-dl integration for git-annex
 -
 - Copyright 2017-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.YoutubeDl (
	youtubeDl,
	youtubeDlTo,
	youtubeDlSupported,
	youtubeDlCheck,
	youtubeDlFileName,
	youtubeDlFileNameHtmlOnly,
) where

import Annex.Common
import qualified Annex
import Annex.Content
import Annex.Url
import Utility.DiskFree
import Utility.HtmlDetect
import Utility.Process.Transcript
import Utility.Metered
import Utility.DataUnits
import Messages.Progress
import Logs.Transfer

import Network.URI
import Control.Concurrent.Async
import Data.Char
import Text.Read

-- youtube-dl can follow redirects to anywhere, including potentially
-- localhost or a private address. So, it's only allowed to download
-- content if the user has allowed access to all addresses.
youtubeDlAllowed :: Annex Bool
youtubeDlAllowed = ipAddressesUnlimited

youtubeDlNotAllowedMessage :: String
youtubeDlNotAllowedMessage = unwords
	[ "This url is supported by youtube-dl, but"
	, "youtube-dl could potentially access any address, and the"
	, "configuration of annex.security.allowed-ip-addresses"
	, "does not allow that. Not using youtube-dl."
	]

-- Runs youtube-dl in a work directory, to download a single media file
-- from the url. Reutrns the path to the media file in the work directory.
--
-- Displays a progress meter as youtube-dl downloads.
--
-- If youtube-dl fails without writing any files to the work directory, 
-- or is not installed, returns Right Nothing.
--
-- The work directory can contain files from a previous run of youtube-dl
-- and it will resume. It should not contain any other files though,
-- and youtube-dl needs to finish up with only one file in the directory
-- so we know which one it downloaded.
--
-- (Note that we can't use --output to specifiy the file to download to,
-- due to <https://github.com/rg3/youtube-dl/issues/14864>)
youtubeDl :: URLString -> FilePath -> MeterUpdate -> Annex (Either String (Maybe FilePath))
youtubeDl url workdir p = ifM ipAddressesUnlimited
	( withUrlOptions $ youtubeDl' url workdir p
	, return $ Left youtubeDlNotAllowedMessage
	)

youtubeDl' :: URLString -> FilePath -> MeterUpdate -> UrlOptions -> Annex (Either String (Maybe FilePath))
youtubeDl' url workdir p uo
	| supportedScheme uo url = ifM (liftIO $ inPath "youtube-dl")
		( runcmd >>= \case
			Right True -> workdirfiles >>= \case
				(f:[]) -> return (Right (Just f))
				[] -> return nofiles
				fs -> return (toomanyfiles fs)
			Right False -> workdirfiles >>= \case
				[] -> return (Right Nothing)
				_ -> return (Left "youtube-dl download is incomplete. Run the command again to resume.")
			Left msg -> return (Left msg)
		, return (Right Nothing)
		)
	| otherwise = return (Right Nothing)
  where
	nofiles = Left "youtube-dl did not put any media in its work directory, perhaps it's been configured to store files somewhere else?"
	toomanyfiles fs = Left $ "youtube-dl downloaded multiple media files; git-annex is only able to deal with one per url: " ++ show fs
	workdirfiles = liftIO $ filterM (doesFileExist) =<< dirContents workdir
	runcmd = youtubeDlMaxSize workdir >>= \case
		Left msg -> return (Left msg)
		Right maxsize -> do
			opts <- youtubeDlOpts (dlopts ++ maxsize)
			oh <- mkOutputHandlerQuiet
			-- The size is unknown to start. Once youtube-dl
			-- outputs some progress, the meter will be updated
			-- with the size, which is why it's important the
			-- meter is passed into commandMeter'
			let unknownsize = Nothing :: Maybe FileSize
			ok <- metered (Just p) unknownsize $ \meter meterupdate ->
				liftIO $ commandMeter' 
					parseYoutubeDlProgress oh (Just meter) meterupdate "youtube-dl" opts
					(\pr -> pr { cwd = Just workdir })
			return (Right ok)
	dlopts = 
		[ Param url
		-- To make youtube-dl only download one file when given a
		-- page with a video and a playlist, download only the video.
		, Param "--no-playlist"
		-- And when given a page with only a playlist, download only
		-- the first video on the playlist. (Assumes the video is
		-- somewhat stable, but this is the only way to prevent
		-- youtube-dl from downloading the whole playlist.)
		, Param "--playlist-items", Param "0"
		]

-- To honor annex.diskreserve, ask youtube-dl to not download too
-- large a media file. Factors in other downloads that are in progress,
-- and any files in the workdir that it may have partially downloaded
-- before.
youtubeDlMaxSize :: FilePath -> Annex (Either String [CommandParam])
youtubeDlMaxSize workdir = ifM (Annex.getState Annex.force)
	( return $ Right []
	, liftIO (getDiskFree workdir) >>= \case
		Just have -> do
			inprogress <- sizeOfDownloadsInProgress (const True)
			partial <- liftIO $ sum 
				<$> (mapM (getFileSize . toRawFilePath) =<< dirContents workdir)
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
				liftIO $ renameFile mediafile dest
				return (Just True)
			Right Nothing -> return (Just False)
			Left msg -> do
				warning msg
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
		liftIO $ snd <$> processTranscript "youtube-dl" (toCommand opts) Nothing
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
		let p = (proc "youtube-dl" (toCommand opts))
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

{- Strategy: Look for chunks prefixed with \r, which look approximately
 - like this:
 - "ESC[K[download]  26.6% of 60.22MiB at 254.69MiB/s ETA 00:00"
 - Look at the number before "% of " and the number and unit after,
 - to determine the number of bytes.
 -}
parseYoutubeDlProgress :: ProgressParser
parseYoutubeDlProgress = go [] . reverse . progresschunks
  where
	delim = '\r'

	progresschunks = drop 1 . splitc delim

	go remainder [] = (Nothing, Nothing, remainder)
	go remainder (x:xs) = case split "% of " x of
		(p:r:[]) -> case (parsepercent p, parsebytes r) of
			(Just percent, Just total) ->
				( Just (toBytesProcessed (calc percent total))
				, Just (TotalSize total)
				, remainder
				)
			_ -> go (delim:x++remainder) xs
		_ -> go (delim:x++remainder) xs

	calc :: Double -> Integer -> Integer
	calc percent total = round (percent * fromIntegral total / 100)

	parsepercent :: String -> Maybe Double
	parsepercent = readMaybe . reverse . takeWhile (not . isSpace) . reverse

	parsebytes = readSize units . takeWhile (not . isSpace)

	units = memoryUnits ++ storageUnits
