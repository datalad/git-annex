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
		opts <- map Param . annexYoutubeDlOptions <$> Annex.getGitConfig
		quiet <- commandProgressDisabled
		let opts' = opts ++
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
		liftIO $ boolSystem' "youtube-dl" opts' $
			\p -> p { cwd = Just workdir }

-- Download a media file to a destination, 
youtubeDlTo :: Key -> URLString -> FilePath -> Annex Bool
youtubeDlTo key url dest = do
	res <- withTmpWorkDir key $ \workdir -> do
		dl <- youtubeDl url workdir
		case dl of
			Right (Just mediafile) -> do
				liftIO $ renameFile mediafile dest
				return (Right True)
			Right Nothing -> return (Right False)
			Left msg -> return (Left msg)
	case res of
		Left msg -> do
			warning msg
			return False
		Right r -> return r

-- Check if youtube-dl can still find media in an url.
youtubeDlSupported :: URLString -> Annex (Either String Bool)
youtubeDlSupported url = liftIO $ catchMsgIO $
	snd <$> processTranscript "youtube-dl" [ url, "--simulate" ] Nothing
