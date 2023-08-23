{- git-annex command
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
import qualified Database.Keys

import qualified Data.ByteString.Char8 as S8

cmd :: Command
cmd = noCommit $
	command "oldkeys" SectionQuery
		"list keys used for old versions of files"
		paramPaths (seek <$$> optParser)

data OldKeysOptions = OldKeysOptions
	{ fileOptions :: CmdParams
	, revisionRange :: Maybe String
	, uncheckedOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser OldKeysOptions
optParser desc = OldKeysOptions
	<$> cmdParams desc
	<*> optional (strOption
		( long "revision-range" <> metavar "RANGE"
		<> help "limit to a revision range"
		))
	<*> switch 
		( long "unchecked"
		<> help "don't check if current files use keys"
		)

seek :: OldKeysOptions -> CommandSeek
seek o = do
	isterminal <- liftIO $ checkIsTerminal stdout
	withdiff $ \l -> 
		forM_ l $ \i ->
			when (DiffTree.srcsha i `notElem` nullShas) $ do
				catKey (DiffTree.srcsha i) >>= \case
					Just key -> commandAction $ 
						start o isterminal key
					Nothing -> return ()
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

start :: OldKeysOptions -> IsTerminal -> Key -> CommandStart
start o (IsTerminal isterminal) key
	| uncheckedOption o = go
	| otherwise = Database.Keys.getAssociatedFiles key >>= \case
		[] -> go
		_ -> stop
  where
	go = startingCustomOutput key $ do
		liftIO $ S8.putStrLn $ if isterminal
			then Utility.Format.encode_c (const False) sk
			else sk
		next $ return True
	sk = serializeKey' key
