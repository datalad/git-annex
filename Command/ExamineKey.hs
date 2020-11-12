{- git-annex command
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.ExamineKey where

import Command
import qualified Utility.Format
import Command.Find (parseFormatOption, showFormatted, keyVars)
import Annex.Link
import Backend
import Types.Backend
import Types.Key

import Data.Char
import qualified Data.ByteString as B

cmd :: Command
cmd = noCommit $ noMessages $ dontCheck repoExists $ 
	withGlobalOptions [jsonOptions] $
		command "examinekey" SectionPlumbing 
			"prints information from a key"
			(paramRepeating paramKey)
			(batchable run optParser)

data ExamineOptions = ExamineOptions
	{ format :: Maybe Utility.Format.Format
	, migrateToBackend :: Maybe (DeferredParse Backend)
	, associatedFile :: AssociatedFile
	}

optParser :: Parser ExamineOptions
optParser = ExamineOptions
	<$> optional parseFormatOption
	<*> (fmap (DeferredParse . tobackend) <$> migrateopt)
	<*> (AssociatedFile <$> fileopt)
  where
	fileopt = optional $ strOption
		( long "filename" <> metavar paramFile
		<> help "file associated with the key"
		)
	migrateopt = optional $ strOption
		( long "migrate-to-backend" <> metavar paramName
		<> help "migrate key to other backend when possible"
		)
	tobackend = lookupBackendVariety . parseKeyVariety . encodeBS

run :: ExamineOptions -> SeekInput -> String -> Annex Bool
run o _ input = do
	k <- getkey
	
	objectpath <- calcRepo $ gitAnnexLocation k
	let objectpointer = formatPointer k
	showFormatted (format o) (serializeKey' k) $
		[ ("objectpath", fromRawFilePath objectpath)
		, ("objectpointer", fromRawFilePath objectpointer)
		] ++ keyVars k
	return True
  where
	-- Parse the input, which is either a key, or in batch mode 
	-- can be "key filename"
	(ikb, ifb) = B.break (== (fromIntegral (ord ' '))) (toRawFilePath input)
	ifb' = B.drop 1 ifb
	ik = fromMaybe (giveup "bad key") (deserializeKey' ikb)
	af = if B.null ifb'
		then associatedFile o
		else AssociatedFile (Just ifb')

	getkey = case migrateToBackend o of
		Nothing -> pure ik
		Just v -> getParsed v >>= \b ->
			maybeLookupBackendVariety (fromKey keyVariety ik) >>= \case
				Just ib -> case fastMigrate ib of
					Just fm -> fromMaybe ik <$> fm ik b af
					Nothing -> pure ik
				Nothing -> pure ik
