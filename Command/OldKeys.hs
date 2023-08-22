{- GIT-annex command
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.OldKeys where

import Command
import Git.Types
import Git.Sha
import qualified Git.Command
import qualified Git.DiffTree as DiffTree
import qualified Annex
import Annex.CatFile
import Utility.Terminal
import qualified Utility.Format

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as S8

cmd :: Command
cmd = noCommit $ withAnnexOptions [annexedMatchingOptions] $
	command "oldkeys" SectionQuery
		"list keys used for old versions of files"
		paramPaths (seek <$$> optParser)

data OldKeysOptions = OldKeysOptions
	{ fileOptions :: CmdParams
	, revisionRange :: Maybe String
	}

optParser :: CmdParamsDesc -> Parser OldKeysOptions
optParser desc = OldKeysOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "revision-range" <> metavar "RANGE"
		<> help "limit to a revision range"
		))

seek :: OldKeysOptions -> CommandSeek
seek o = do
	isterminal <- liftIO $ checkIsTerminal stdout
	-- Get the diff twice and make separate passes over it
	-- to avoid needing to cache it all in memory.
	currentkeys <- withdiff getcurrentkeys
	withdiff $ \l -> 
		forM_ l $ \i ->
			when (DiffTree.srcsha i `notElem` nullShas) $ do
				catKey (DiffTree.srcsha i) >>= \case
					Just key | S.notMember key currentkeys ->
						commandAction $ start isterminal key
					_ -> return ()
  where
  	withdiff a = do
		(output, cleanup) <- Annex.inRepo $
			Git.Command.pipeNullSplit ps
		let l = filter (isfilemode . DiffTree.srcmode)
			(DiffTree.parseDiffRaw output)
		r <- a l
		liftIO $ void cleanup
		return r
	
	ps = 
		[ Param "log"
		, Param "-z"
		-- Don't convert pointer files.
		, Param "--no-textconv"
		-- Don't abbreviate hashes.
		, Param "--no-abbrev"
		-- Don't show renames.
		, Param "--no-renames"
		-- Output the raw diff.
		, Param "--raw"
		-- Avoid outputting anything except for the raw diff.
		, Param "--pretty="
		]
		++ case revisionRange o of
			Nothing -> []
			Just rr -> [Param rr]
		++ map File (fileOptions o)
	
	isfilemode m = case toTreeItemType m of
		Just TreeFile -> True
		Just TreeExecutable -> True
		Just TreeSymlink -> True
		_ -> False
	
	-- Accumulate the most recent key used for each file 
	-- (that is not deleted).
	-- Those keys should never be listed as old keys, even if
	-- some other file did have them as an old key. This avoids
	-- surprising behavior for renames and reverts.
	getcurrentkeys l = getcurrentkeys' l M.empty
	getcurrentkeys' [] m = pure $ S.fromList $ catMaybes $ M.elems m
	getcurrentkeys' (i:l) m
		| not (isfilemode (DiffTree.dstmode i)) =
			getcurrentkeys' l m
		| DiffTree.dstsha i `elem` nullShas = 
			getcurrentkeys' l $
				M.insertWith (\_ prev -> prev)
					(DiffTree.file i)
					Nothing
					m
		| otherwise = case M.lookup (DiffTree.file i) m of
			Just _ -> getcurrentkeys' l m
			Nothing -> catKey (DiffTree.dstsha i) >>= \case
				Just key -> getcurrentkeys' l $
					M.insert	
						(DiffTree.file i)
						(Just key)
						m
				_ -> getcurrentkeys' l m

start :: IsTerminal -> Key -> CommandStart
start (IsTerminal isterminal) key = startingCustomOutput key $ do
	liftIO $ S8.putStrLn $ if isterminal
		then Utility.Format.encode_c (const False) sk
		else sk
	next $ return True
  where
	sk = serializeKey' key
