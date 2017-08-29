{- git-annex command
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Export where

import Command
import qualified Git
import qualified Git.DiffTree
import Git.Sha
import Git.FilePath
import Types.Key
import Types.Remote
import Annex.Content
import Annex.CatFile
import Messages.Progress
import Utility.Tmp

import qualified Data.ByteString.Lazy as L

cmd :: Command
cmd = command "export" SectionCommon
	"export content to a remote"
	paramTreeish (seek <$$> optParser)

data ExportOptions = ExportOptions
	{ exportTreeish :: Git.Ref
	, exportRemote :: DeferredParse Remote
	}

optParser :: CmdParamsDesc -> Parser ExportOptions
optParser _ = ExportOptions
	<$> (Git.Ref <$> parsetreeish)
	<*> (parseRemoteOption <$> parseToOption)
  where
	parsetreeish = argument str
		( metavar paramTreeish
		)

seek :: ExportOptions -> CommandSeek
seek o = do
	r <- getParsed (exportRemote o)
	let oldtreeish = emptyTree -- XXX temporary
	(diff, cleanup) <- inRepo $
		Git.DiffTree.diffTreeRecursive oldtreeish (exportTreeish o)
	seekActions $ pure $ map (start r) diff
	void $ liftIO cleanup

start :: Remote -> Git.DiffTree.DiffTreeItem -> CommandStart
start r diff
	| Git.DiffTree.dstsha diff == nullSha = do
		showStart "unexport" f
		oldk <- either id id <$> exportKey (Git.DiffTree.srcsha diff)
		next $ performUnexport r oldk loc
	| otherwise = do
		showStart "export" f
		next $ performExport r diff loc
  where
	loc = ExportLocation $ toInternalGitPath $ 
		getTopFilePath $ Git.DiffTree.file diff
	f = getTopFilePath $ Git.DiffTree.file diff

performExport :: Remote -> Git.DiffTree.DiffTreeItem -> ExportLocation -> CommandPerform
performExport r diff loc = case storeExport r of
	Nothing -> error "remote does not support exporting files"
	Just storer -> next $ do
		v <- exportKey (Git.DiffTree.dstsha diff)
		case v of
			Right k -> metered Nothing k $ \m ->
				sendAnnex k
					(void $ performUnexport r k loc)
					(\f -> storer f k loc m)
			-- Sending a non-annexed file.
			Left sha1k -> metered Nothing sha1k $ \m ->
				withTmpFile "export" $ \tmp h -> do
					b <- catObject (Git.DiffTree.dstsha diff)
					liftIO $ L.hPut h b
					liftIO $ hClose h
					storer tmp sha1k loc m

performUnexport :: Remote -> Key -> ExportLocation -> CommandPerform
performUnexport r k loc = case removeExport r of
	Nothing -> error "remote does not support removing exported files"
	Just remover -> next $ remover k loc

-- When the Sha points to an annexed file, get the key as Right.
-- When the Sha points to a non-annexed file, convert to a SHA1 key,
-- as Left.
exportKey :: Git.Sha -> Annex (Either Key Key)
exportKey sha = mk <$> catKey sha
  where
	mk (Just k) = Right k
	mk Nothing = Left $ Key
		{ keyName = show sha
		, keyVariety = SHA1Key (HasExt False)
		, keySize = Nothing
		, keyMtime = Nothing
		, keyChunkSize = Nothing
		, keyChunkNum = Nothing
		}
