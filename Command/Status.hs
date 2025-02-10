{- git-annex command
 -
 - Copyright 2013-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Status where

import Command
import qualified Annex
import Git.Status
import Git.FilePath

import Data.ByteString.Char8 as B8

cmd :: Command
cmd = notBareRepo $ noCommit $ noMessages $
	withAnnexOptions [jsonOptions] $
		command "status" SectionCommon
			"show the working tree status (deprecated)"
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
	let getstatus = pure . simplifiedStatus
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
	qp <- coreQuotePath <$> Annex.getGitConfig
	unlessM (showFullJSON $ JSONChunk [("status", [c]), ("file", fromOsPath f)]) $
		liftIO $ B8.putStrLn $ quote qp $
			UnquotedString (c : " ") <> QuotedPath f
