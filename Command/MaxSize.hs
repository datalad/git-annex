{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.MaxSize where

import Command
import qualified Remote
import Annex.RepoSize
import Types.RepoSize
import Logs.MaxSize
import Logs.Trust
import Utility.DataUnits
import Utility.Percentage

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

cmd :: Command
cmd = noMessages $ withAnnexOptions [jsonOptions] $ 
	command "maxsize" SectionSetup
		"configure maximum size of repositoriy"
		(paramPair paramRepository (paramOptional paramSize))
		(seek <$$> optParser)

data MaxSizeOptions = MaxSizeOptions
	{ cmdparams :: CmdParams
	, bytesOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser MaxSizeOptions
optParser desc = MaxSizeOptions
	<$> cmdParams desc
	<*> switch
		( long "bytes"
		<> help "display sizes in bytes"
		)

seek :: MaxSizeOptions -> CommandSeek
seek o = case cmdparams o of
	(rname:[]) -> commandAction $ do
		enableNormalOutput
		showCustom "maxsize" (SeekInput [rname]) $ do
			u <- Remote.nameToUUID rname
			v <- M.lookup u <$> getMaxSizes
			maybeAddJSONField "maxsize" (fromMaxSize <$> v)
			showRaw $ encodeBS $ case v of
				Just (MaxSize n) -> 
					formatSize o (preciseSize storageUnits True) n
				Nothing -> ""
			return True
		stop
	(rname:sz:[]) -> commandAction $ do
		u <- Remote.nameToUUID rname
		let si = SeekInput (cmdparams o)
		let ai = ActionItemOther (Just (UnquotedString rname))
		startingUsualMessages "maxsize" ai si $
			case readSize dataUnits sz of
				Nothing -> giveup "Unable to parse size."
				Just n -> do
					recordMaxSize u (MaxSize n)
					next $ return True
	[] -> commandAction $ sizeOverview o
	_ -> giveup "Too many parameters"

sizeOverview :: MaxSizeOptions -> CommandStart
sizeOverview o = do
	enableNormalOutput
	showCustom "maxsize" (SeekInput []) $ do
		descmap <- Remote.uuidDescriptions
		deadset <- S.fromList <$> trustGet DeadTrusted
		maxsizes <- getMaxSizes
		reposizes <- flip M.withoutKeys deadset <$> getRepoSizes True
		let l = reverse $ sortOn snd $ M.toList $
			M.mapWithKey (gather maxsizes) reposizes
		v <- Remote.prettyPrintUUIDsWith' False (Just "size")
		 	"repositories" descmap showsizes l
		showRaw $ encodeBS $ tablerow (zip widths headers)
		showRaw $ encodeBS $ dropWhileEnd (== '\n') v
		return True
	stop
  where
	sizefield = "size" :: T.Text
	maxsizefield = "maxsize" :: T.Text
	
	gather maxsizes u (RepoSize currsize) = Just $
			M.fromList
				[ (sizefield, Just currsize)
				, (maxsizefield, fromMaxSize <$> M.lookup u maxsizes)
				]

	(widths, headers) = unzip
		[ (7, "size")
		, (7, "maxsize")
		, (6, "%full")
		, (0, "repository")
		]
	
	showsizes m = do
		size <- M.lookup sizefield m
		maxsize <- M.lookup maxsizefield m
		return $ tablerow $ zip widths
			[ formatsize size
			, formatsize maxsize
			, case (size, maxsize) of
				(Just size', Just maxsize')
					| size' <= maxsize' ->
						showPercentage 0 $
							percentage maxsize' size'
					| otherwise -> ">100%"
				_ -> ""
			, ""
			]
	
	formatsize = maybe "" (formatSize o (roughSize' storageUnits True 0))

	padcolumn width s = replicate (width - length s) ' ' ++ s
	
	tablerow [] = ""
	tablerow ((_, s):[]) = " " ++ s
	tablerow ((width, s):l) = padcolumn width s ++ " " ++ tablerow l
					
formatSize :: MaxSizeOptions -> (ByteSize -> String) -> ByteSize -> String
formatSize o f n
	| bytesOption o = show n
	| otherwise = f n
