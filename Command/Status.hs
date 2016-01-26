{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Status where

import Command
import Annex.CatFile
import Annex.Content.Direct
import Config
import Git.Status
import qualified Git.Ref
import Git.FilePath

cmd :: Command
cmd = notBareRepo $ noCommit $ noMessages $
	withGlobalOptions [jsonOption] $
		command "status" SectionCommon
			"show the working tree status"
			paramPaths (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start
	
start :: [FilePath] -> CommandStart
start locs = do
	(l, cleanup) <- inRepo $ getStatus locs
	getstatus <- ifM isDirect
		( return statusDirect
		, return $ \s -> pure (Just s)
		)
	forM_ l $ \s -> maybe noop displayStatus =<< getstatus s
	void $ liftIO cleanup
	stop

displayStatus :: Status -> Annex ()
-- renames not shown in this simplified status
displayStatus (Renamed _ _) = noop
displayStatus s  = do
	let c = statusChar s
	absf <- fromRepo $ fromTopFilePath (statusFile s)
	f <- liftIO $ relPathCwdToFile absf
	unlessM (showFullJSON [("status", [c]), ("file", f)]) $
		liftIO $ putStrLn $ [c] ++ " " ++ f

-- Git thinks that present direct mode files are typechanged.
-- (On crippled filesystems, git instead thinks they're modified.)
-- Check their content to see if they are modified or not.
statusDirect :: Status -> Annex (Maybe Status)
statusDirect (TypeChanged t) = statusDirect' t
statusDirect s@(Modified t) = ifM crippledFileSystem
	( statusDirect' t
	, pure (Just s)
	)
statusDirect s = pure (Just s)

statusDirect' :: TopFilePath -> Annex (Maybe Status)
statusDirect' t = do
	absf <- fromRepo $ fromTopFilePath t
	f <- liftIO $ relPathCwdToFile absf
	v <- liftIO (catchMaybeIO $ getFileStatus f)
	case v  of
		Nothing -> return $ Just $ Deleted t
		Just s
			| not (isSymbolicLink s) ->
				checkkey f s =<< catKeyFile f
			| otherwise -> Just <$> checkNew f t
  where
	checkkey f s (Just k) = ifM (sameFileStatus k f s)
		( return Nothing
		, return $ Just $ Modified t
		)
	checkkey f _ Nothing = Just <$> checkNew f t

checkNew :: FilePath -> TopFilePath -> Annex Status
checkNew f t = ifM (isJust <$> catObjectDetails (Git.Ref.fileRef f))
	( return (Modified t)
	, return (Untracked t)
	)
