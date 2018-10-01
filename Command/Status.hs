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
	withGlobalOptions [jsonOptions] $
		command "status" SectionCommon
			"show the working tree status"
			paramPaths (seek <$$> optParser)

data StatusOptions = StatusOptions
	{ statusFiles :: CmdParams
	, ignoreSubmodules :: Maybe String
	}

optParser :: CmdParamsDesc -> Parser StatusOptions
optParser desc = StatusOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "ignore-submodules"
		<> help "passed on to git status"
		<> metavar "WHEN"
		))

seek :: StatusOptions -> CommandSeek
seek o = withWords (commandAction . start o) (statusFiles o)
	
start :: StatusOptions -> [FilePath] -> CommandStart
start o locs = do
	(l, cleanup) <- inRepo $ getStatus ps locs
	getstatus <- ifM isDirect
		( return (maybe (pure Nothing) statusDirect . simplifiedStatus)
		, return (pure . simplifiedStatus)
		)
	forM_ l $ \s -> maybe noop displayStatus =<< getstatus s
	ifM (liftIO cleanup)
		( stop
		, giveup "git status failed"
		)
  where
	ps = case ignoreSubmodules o of
		Nothing -> []
		Just s -> [Param $ "--ignore-submodules="++s]

-- Prefer to show unstaged status in this simplified status.
simplifiedStatus :: StagedUnstaged Status -> Maybe Status
simplifiedStatus (StagedUnstaged { unstaged = Just s }) = Just s
simplifiedStatus (StagedUnstaged { staged = Just s }) = Just s
simplifiedStatus _ = Nothing

displayStatus :: Status -> Annex ()
-- Renames not shown in this simplified status
displayStatus (Renamed _ _) = noop
displayStatus s = do
	let c = statusChar s
	absf <- fromRepo $ fromTopFilePath (statusFile s)
	f <- liftIO $ relPathCwdToFile absf
	unlessM (showFullJSON $ JSONChunk [("status", [c]), ("file", f)]) $
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
