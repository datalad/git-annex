{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.WhereUsed where

import Command
import Git
import Git.Sha
import Git.FilePath
import qualified Git.Command
import qualified Git.DiffTree as DiffTree
import qualified Annex
import qualified Annex.Branch
import Annex.CatFile
import Database.Keys

import Data.Char
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = noCommit $ withAnnexOptions [annexedMatchingOptions] $
	command "whereused" SectionQuery
		"lists repositories that have file content"
		paramNothing (seek <$$> optParser)

data WhereUsedOptions = WhereUsedOptions
	{ keyOptions :: KeyOptions
	, historicalOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser WhereUsedOptions
optParser _desc = WhereUsedOptions
	<$> (parseUnusedKeysOption <|> parseSpecificKeyOption)
	<*> switch
		( long "historical"
		<> help "find historical uses"
		)

seek :: WhereUsedOptions -> CommandSeek
seek o = withKeyOptions (Just (keyOptions o)) False dummyfileseeker
	(commandAction . start o) dummyfilecommandseek (WorkTreeItems [])
  where
	dummyfileseeker = AnnexedFileSeeker
		{ startAction = \_ _ _ _ -> return Nothing
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}
	dummyfilecommandseek = const noop

start :: WhereUsedOptions -> (SeekInput, Key, ActionItem) -> CommandStart
start o (_, key, _) = startingCustomOutput key $ do
	fs <- filterM stillassociated
		=<< mapM (liftIO . relPathCwdToFile)
		=<< mapM (fromRepo . fromTopFilePath)
		=<< getAssociatedFiles key
	forM_ fs $ display key . QuotedPath

	when (historicalOption o && null fs) $
		findHistorical key

	next $ return True
  where
	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository.
	stillassociated f = catKeyFile f >>= \case
		Just k | k == key -> return True
		_ -> return False

display :: Key -> StringContainingQuotedPath -> Annex ()
display key loc = do
	qp <- coreQuotePath <$> Annex.getGitConfig
	liftIO $ S8.putStrLn $ quote qp $
		UnquotedByteString (serializeKey' key) <> " " <> loc

findHistorical :: Key -> Annex ()
findHistorical key = do
	-- Find most recent change to the key, in all branches and
	-- tags, except the git-annex branch.
	found <- searchLog key 
		-- Search all local branches, except git-annex branch.
		[ Param ("--exclude=*/" ++ fromRef (Annex.Branch.name))
		, Param "--glob=*"
		-- Also search remote branches
		, Param ("--exclude=" ++ fromRef (Annex.Branch.name))
		, Param "--remotes=*"
		-- And search tags.
		, Param "--tags=*"
		-- Output the commit hash
		, Param "--pretty=%H"
		] $ \h fs -> do
			commitsha <- liftIO $ getSha "log" (pure h)
			commitdesc <- S.takeWhile (/= fromIntegral (ord '\n'))
				<$> inRepo (Git.Command.pipeReadStrict
					[ Param "describe"
					, Param "--contains"
					, Param "--all"
					, Param (fromRef commitsha)
					])
			if S.null commitdesc
				then return False
				else process fs $
					displayreffile (Ref commitdesc)
		
	unless found $
		void $ searchLog key
			[ Param "--walk-reflogs"
			-- Output the reflog selector
			, Param "--pretty=%gd"
			] $ \h fs -> process fs $
				displayreffile (Ref h)
  where
	process fs a = or <$> forM fs a

	displayreffile r f = do
		tf <- inRepo $ toTopFilePath f
		display key (descBranchFilePath (BranchFilePath r tf))
		return True

searchLog :: Key -> [CommandParam] -> (S.ByteString -> [OsPath] -> Annex Bool) -> Annex Bool
searchLog key ps a = do
	(output, cleanup) <- Annex.inRepo $ Git.Command.pipeNullSplit ps'
	found <- case output of
		(h:rest) -> do
			let diff = DiffTree.parseDiffRaw rest
			repo <- Annex.gitRepo
			let fs = map (flip fromTopFilePath repo . DiffTree.file) diff
			rfs <- liftIO $ mapM relPathCwdToFile fs
			a (L.toStrict h) rfs
		_ -> return False
	liftIO $ void cleanup
	return found
  where
	ps' = 
		[ Param "log"
		, Param "-z"
		-- Don't convert pointer files.
		, Param "--no-textconv"
		-- Don't abbreviate hashes.
		, Param "--no-abbrev"
		-- Only find the most recent commit, for speed.
		, Param "-n1"
		-- Be sure to treat -G as a regexp.
		, Param "--basic-regexp"
		-- Find commits that contain the key. The object has to
		-- end with the key to avoid confusion with longer keys,
		-- so a regexp is used. Since annex pointer files
		-- may contain a newline followed by perhaps something
		-- else, that is also matched.
		, Param ("-G" ++ escapeRegexp (fromOsPath (keyFile key)) ++ "($|\n)")
		-- Skip commits where the file was deleted,
		-- only find those where it was added or modified.
		, Param "--diff-filter=ACMRTUX"
		-- Output the raw diff.
		, Param "--raw"
		] ++ ps

escapeRegexp :: String -> String
escapeRegexp = concatMap esc
  where
	esc c
		| isAscii c && isAlphaNum c = [c]
		| otherwise = ['[', c, ']']
