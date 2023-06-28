{- git-annex command
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.DiffDriver where

import Command
import Annex.Content
import Annex.Link
import Git.Types

cmd :: Command
cmd = dontCheck repoExists $
	command "diffdriver" SectionPlumbing 
		"git diff driver"
		("-- cmd --") (seek <$$> optParser)

data Options = Options
	{ textDiff :: Bool
	, restOptions :: CmdParams
	}

optParser :: CmdParamsDesc -> Parser Options
optParser desc = Options
	<$> switch
		( long "text"
		<> help "diff text files with diff(1)"
		)
	<*> cmdParams desc

seek :: Options -> CommandSeek
seek = commandAction . start

start :: Options -> CommandStart
start opts = do
	let (req, differ) = parseReq opts
	void $ liftIO . exitBool =<< liftIO . differ =<< fixupReq req
	stop

data Req 
	= Req
		{ rPath :: FilePath
		, rOldFile :: FilePath
		, rOldHex :: String
		, rOldMode :: String
		, rNewFile :: FilePath
		, rNewHex :: String
		, rNewMode ::String
		}
	| UnmergedReq
		{ rPath :: FilePath
		}

type Differ = Req -> IO Bool

serializeReq :: Req -> [CommandParam]
serializeReq req@(UnmergedReq {}) = [Param $ rPath req]
serializeReq req@(Req {}) = map Param
	[ rPath req
	, rOldFile req
	, rOldHex req
	, rOldMode req
	, rNewFile req
	, rNewHex req
	, rNewMode req
	]

parseReq :: Options -> (Req, Differ)
parseReq opts
	| textDiff opts = (mk (restOptions opts), textDiffer)
	| otherwise = case separate (== "--") (restOptions opts) of
		(c:ps, l) -> (mk l, externalDiffer c ps)
		([],_) -> badopts
  where
	mk (path:old_file:old_hex:old_mode:new_file:new_hex:new_mode:[]) =
		Req
			{ rPath = path
			, rOldFile = old_file
			, rOldHex = old_hex
			, rOldMode = old_mode
			, rNewFile = new_file
			, rNewHex = new_hex
			, rNewMode = new_mode
			}
	mk (unmergedpath:[]) = UnmergedReq { rPath = unmergedpath }
	mk _ = badopts

	badopts = giveup $ "Unexpected input: " ++ unwords (restOptions opts)

{- Check if either file is a symlink to a git-annex object,
 - which git-diff will leave as a normal file containing the link text.
 -
 - Also check if either file is a pointer file, as used for unlocked files.
 -
 - In either case, adjust the Req to instead point to the actual
 - location of the annexed object (which may or may not be present).
 -}
fixupReq :: Req -> Annex Req
fixupReq req@(UnmergedReq {}) = return req
fixupReq req@(Req {}) = 
	check rOldFile rOldMode (\r f -> r { rOldFile = f }) req
		>>= check rNewFile rNewMode (\r f -> r { rNewFile = f })
  where
	check getfile getmode setfile r = case readTreeItemType (encodeBS (getmode r)) of
		Just TreeSymlink -> do
			v <- getAnnexLinkTarget' f False
			maybe (return r) repoint (parseLinkTargetOrPointer =<< v)
		_ -> maybe (return r) repoint =<< liftIO (isPointerFile f)
	  where
		repoint k = withObjectLoc k $
			pure . setfile r . fromRawFilePath
		f = toRawFilePath (getfile r)

externalDiffer :: String -> [String] -> Differ
externalDiffer c ps = \req -> boolSystem c (map Param ps ++ serializeReq req )

textDiffer :: Differ
textDiffer req = do
	putStrLn ("diff a/" ++ rPath req ++ " b/" ++ rPath req)
	-- diff exits nonzero on difference, so ignore exit status
	void $ boolSystem "diff"
		[ Param "-u"
		, Param (rOldFile req)
		, Param (rNewFile req)
		]
	return True
