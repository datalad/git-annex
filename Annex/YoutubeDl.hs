{- youtube-dl integration for git-annex
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.YoutubeDl where

import Annex.Common
import qualified Annex
import Annex.Content
import Utility.Url (URLString)

-- Runs youtube-dl in a work directory, to download a single media file
-- from the url. Reutrns the path to the media file in the work directory.
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
youtubeDl :: URLString -> FilePath -> Annex (Either String (Maybe FilePath))
youtubeDl url workdir = ifM (liftIO (inPath "youtube-dl") <&&> runcmd)
	( do
		fs <- liftIO $ filterM (doesFileExist) =<< dirContents workdir
		case fs of
			(f:[]) -> return (Right (Just f))
			[] -> return nofiles
			_ -> return (toomanyfiles fs)
	, do
		fs <- liftIO $ filterM (doesFileExist) =<< dirContents workdir
		if null fs
			then return (Right Nothing)
			else return (Left "youtube-dl download is incomplete. Run the command again to resume.")
	)
  where
	nofiles = Left "youtube-dl did not put any media in its work directory, perhaps it's been configured to store files somewhere else?"
	toomanyfiles fs = Left $ "youtube-dl downloaded multiple media files; git-annex is only able to deal with one per url: " ++ show fs
	runcmd = do
		quiet <- commandProgressDisabled
		opts <- youtubeDlOpts $
			[ Param url
			-- To make youtube-dl only download one file,
			-- when given a page with a video and a playlist,
			-- download only the video.
			, Param "--no-playlist"
			-- And when given a page with only a playlist,
			-- download only the first video on the playlist.
			-- (Assumes the video is somewhat stable, but
			-- this is the only way to prevent youtube-dl
			-- from downloading the whole playlist.)
			, Param "--playlist-items", Param "0"
			-- TODO --max-filesize
			] ++
			if quiet then [ Param "--quiet" ] else []
		liftIO $ boolSystem' "youtube-dl" opts $
			\p -> p { cwd = Just workdir }

-- Download a media file to a destination, 
youtubeDlTo :: Key -> URLString -> FilePath -> Annex Bool
youtubeDlTo key url dest = do
	res <- withTmpWorkDir key $ \workdir -> do
		dl <- youtubeDl url workdir
		case dl of
			Right (Just mediafile) -> do
				liftIO $ renameFile mediafile dest
				return (Just True)
			Right Nothing -> return (Just False)
			Left msg -> do
				warning msg
				return Nothing
	return (fromMaybe False res)

youtubeDlSupported :: URLString -> Annex Bool
youtubeDlSupported url = either (const False) id <$> youtubeDlCheck url

-- Check if youtube-dl can find media in an url.
youtubeDlCheck :: URLString -> Annex (Either String Bool)
youtubeDlCheck url = catchMsgIO $ do
	opts <- youtubeDlOpts [ Param url, Param "--simulate" ]
	liftIO $ snd <$> processTranscript "youtube-dl" (toCommand opts) Nothing

-- Ask youtube-dl for the filename of media in an url.
--
-- (This is not always identical to the filename it uses when downloading.)
youtubeDlFileName :: URLString -> Annex (Either String FilePath)
youtubeDlFileName url = flip catchIO (pure . Left . show) $ do
	-- Sometimes youtube-dl will fail with an ugly backtrace
	-- (eg, http://bugs.debian.org/874321)
	-- so catch stderr as well as stdout to avoid the user seeing it. 
	-- --no-warnings avoids warning messages that are output to stdout.
	opts <- youtubeDlOpts
		[ Param url
		, Param "--get-filename"
		, Param "--no-warnings"
		]
	(output, ok) <- liftIO $ processTranscript "youtube-dl" (toCommand opts) Nothing
	return $ case (ok, lines output) of
		(True, (f:_)) | not (null f) -> Right f
		_ -> Left "no media in url"

youtubeDlOpts :: [CommandParam] -> Annex [CommandParam]
youtubeDlOpts addopts = do
	opts <- map Param . annexYoutubeDlOptions <$> Annex.getGitConfig
	return (opts ++ addopts)
