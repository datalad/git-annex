{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Status where

import Common.Annex
import Command
import Annex.CatFile
import Annex.Content.Direct
import Config
import qualified Git.LsFiles as LsFiles
import qualified Git.Ref
import qualified Git

def :: [Command]
def = [notBareRepo $ noCommit $ noMessages $ withOptions [jsonOption] $
	command "status" paramPaths seek SectionCommon
		"show the working tree status"]

seek :: CommandSeek
seek = withWords start

start :: [FilePath] -> CommandStart
start [] = do
	-- Like git status, when run without a directory, behave as if
	-- given the path to the top of the repository.
	cwd <- liftIO getCurrentDirectory
	top <- fromRepo Git.repoPath
	start' [relPathDirToFile cwd top]
start locs = start' locs
	
start' :: [FilePath] -> CommandStart
start' locs = do
	(l, cleanup) <- inRepo $ LsFiles.modifiedOthers locs
	getstatus <- ifM isDirect
		( return statusDirect
		, return $ Just <$$> statusIndirect
		)
	forM_ l $ \f -> maybe noop (showFileStatus f) =<< getstatus f
	void $ liftIO cleanup
	stop

data Status 
	= NewFile
	| DeletedFile
	| ModifiedFile

showStatus :: Status -> String
showStatus NewFile = "?"
showStatus DeletedFile = "D"
showStatus ModifiedFile = "M"

showFileStatus :: FilePath -> Status -> Annex ()
showFileStatus f s  = unlessM (showFullJSON [("status", ss), ("file", f)]) $
	liftIO $ putStrLn $ ss ++ " " ++ f
  where
	ss = showStatus s

statusDirect :: FilePath -> Annex (Maybe Status)
statusDirect f = checkstatus =<< liftIO (catchMaybeIO $ getFileStatus f)
  where
	checkstatus Nothing = return $ Just DeletedFile
	checkstatus (Just s)
		-- Git thinks that present direct mode files modifed,
		-- so have to check.
		| not (isSymbolicLink s) = checkkey s =<< catKeyFile f
		| otherwise = Just <$> checkNew f
	
	checkkey s (Just k) = ifM (sameFileStatus k s)
		( return Nothing
		, return $ Just ModifiedFile
		)
	checkkey _ Nothing = Just <$> checkNew f

statusIndirect :: FilePath -> Annex Status
statusIndirect f = ifM (liftIO $ isJust <$> catchMaybeIO (getFileStatus f))
	( checkNew f
	, return DeletedFile
	)
  where

checkNew :: FilePath -> Annex Status
checkNew f = ifM (isJust <$> catObjectDetails (Git.Ref.fileRef f))
	( return ModifiedFile
	, return NewFile
	)
