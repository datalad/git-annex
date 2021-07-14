{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.WhereUsed where

import Command
import Git
import Git.Sha
import Git.FilePath
import qualified Git.Ref
import qualified Git.Command
import qualified Git.DiffTree as DiffTree
import qualified Annex
import qualified Annex.Branch
import Annex.CatFile
import Database.Keys

import Data.Char
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = noCommit $ withGlobalOptions [annexedMatchingOptions] $
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
		{ startAction = \_ _ _ -> return Nothing
		, checkContentPresent = Nothing
		, usesLocationLog = False
		}
	dummyfilecommandseek = const noop

start :: WhereUsedOptions -> (SeekInput, Key, ActionItem) -> CommandStart
start o (_, key, _) = startingCustomOutput key $ do
	fs <- filterM stillassociated 
		=<< mapM (fromRepo . fromTopFilePath)
		=<< getAssociatedFiles key
	liftIO $ forM_ fs $ display key . fromRawFilePath

	when (historicalOption o && null fs) $
		findHistorical key

	next $ return True
  where
	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository.
	stillassociated f = catKeyFile f >>= \case
		Just k | k == key -> return True
		_ -> return False

display :: Key -> String -> IO ()
display key loc = putStrLn (serializeKey key ++ " " ++ loc)

findHistorical :: Key -> Annex ()
findHistorical key = do
	Annex.inRepo $ \repo -> do
		-- Find most recent change to the key, in all branches and
		-- tags, except the git-annex branch.
		(output, cleanup) <- Git.Command.pipeNullSplit 
			[ Param "log"
			, Param "-z"
			-- Don't convert pointer files.
			, Param "--no-textconv"
			-- Only find the most recent commit, for speed.
			, Param "-n1"
			-- Find commits that contain the key.
			, Param ("-S" ++ fromRawFilePath (keyFile key))
			-- Skip commits where the file was deleted,
			-- only find those where it was added or modified.
			, Param "--diff-filter=ACMRTUX"
			-- Search all local branches, except git-annex branch.
			, Param ("--exclude=*/" ++ fromRef (Annex.Branch.name))
			, Param "--glob=*"
			-- Also search remote branches
			, Param ("--exclude=" ++ fromRef (Annex.Branch.name))
			, Param "--remotes=*"
			-- And search tags.
			, Param "--tags=*"
			-- Output the commit hash
			, Param "--pretty=%H"
			-- And the raw diff.
			, Param "--raw"
			-- Don't abbreviate hashes.
			, Param "--no-abbrev"
			] repo
		found <- case output of
			(h:rest) -> do
				commitsha <- getSha "log" (pure (L.toStrict h))
				let diff = DiffTree.parseDiffRaw rest
				forM_ (map (flip fromTopFilePath repo . DiffTree.file) diff) $ \f -> do
					commitdesc <- S.takeWhile (/= fromIntegral (ord '\n'))
						<$> Git.Command.pipeReadStrict
							[ Param "describe"
							, Param "--contains"
							, Param "--all"
							, Param (fromRef commitsha)
							] repo
					if S.null commitdesc
						then return False
						else do
							rf <- relPathCwdToFile f
							let fref = Git.Ref.branchFileRef (Ref commitdesc) rf
							display key (fromRef fref)
							return True
			_ -> return False
		void cleanup
		
		unless found $ do
			error "todo reflog"
