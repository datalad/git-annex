{- git-annex command
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TupleSections #-}

module Command.Whereis where

import Command
import Remote
import Logs.Trust
import Logs.Web
import Remote.Web (getWebUrls)
import Annex.UUID
import qualified Utility.Format
import qualified Command.Find
import Types.ActionItem

import qualified Data.Map as M
import qualified Data.Vector as V

cmd :: Command
cmd = noCommit $ withGlobalOptions [jsonOptions, annexedMatchingOptions] $
	command "whereis" SectionQuery
		"lists repositories that have file content"
		paramPaths (seek <$$> optParser)

data WhereisOptions = WhereisOptions
	{ whereisFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	, formatOption :: Maybe Utility.Format.Format
	}

optParser :: CmdParamsDesc -> Parser WhereisOptions
optParser desc = WhereisOptions
	<$> cmdParams desc
	<*> optional parseKeyOptions
	<*> parseBatchOption
	<*> optional parseFormatOption

parseFormatOption :: Parser Utility.Format.Format
parseFormatOption = option (Utility.Format.gen <$> str)
	( long "format" <> metavar paramFormat
	<> help "control format of output"
	)

seek :: WhereisOptions -> CommandSeek
seek o = do
	m <- remoteMap id
	let seeker = AnnexedFileSeeker
		{ startAction = start o m
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	case batchOption o of
		NoBatch -> do
			withKeyOptions (keyOptions o) False seeker
				(commandAction . startKeys o m)
				(withFilesInGitAnnex ww seeker)
				=<< workTreeItems ww (whereisFiles o)
		Batch fmt -> batchAnnexedFilesMatching fmt seeker
  where
	ww = WarnUnmatchLsFiles

start :: WhereisOptions -> M.Map UUID Remote -> RawFilePath -> Key -> CommandStart
start o remotemap file key = 
	startKeys o remotemap (key, mkActionItem (key, afile))
  where
	afile = AssociatedFile (Just file)

startKeys :: WhereisOptions -> M.Map UUID Remote -> (Key, ActionItem) -> CommandStart
startKeys o remotemap (key, ai)
	| isJust (formatOption o) = startingCustomOutput ai go
	| otherwise = starting "whereis" ai go
  where
	go = perform o remotemap key ai

perform :: WhereisOptions -> M.Map UUID Remote -> Key -> ActionItem -> CommandPerform
perform o remotemap key ai = do
	locations <- keyLocations key
	urls <- getUUIDUrls key locations remotemap
	(untrustedlocations, safelocations) <- trustPartition UnTrusted locations
	case formatOption o of
		Nothing -> do
			let num = length safelocations
			showNote $ show num ++ " " ++ copiesplural num
			pp <- ppwhereis "whereis" safelocations urls
			unless (null safelocations) $ showLongNote pp
			pp' <- ppwhereis "untrusted" untrustedlocations urls
			unless (null untrustedlocations) $ showLongNote $ untrustedheader ++ pp'
		
			mapM_ (showRemoteUrls remotemap) urls
		Just formatter -> liftIO $ do
			let vs = catMaybes
				[ fmap (("file",) . fromRawFilePath)
					(actionItemWorkTreeFile ai)
				] ++ Command.Find.keyVars key
			let showformatted muuid murl = putStr $
				Utility.Format.format formatter $
					M.fromList $ vs ++ catMaybes
						[ fmap ("uuid",) muuid
						, fmap ("url",) murl
						]
			let showformatted' muuid
				| Utility.Format.formatContainsVar "url" formatter =
					forM_ (concatMap snd urls) $ 
						showformatted muuid . Just
				| otherwise = showformatted muuid Nothing
			if Utility.Format.formatContainsVar "uuid" formatter
				then forM_ locations $
					showformatted' . Just . fromUUID
				else showformatted' Nothing

	if null safelocations then stop else next $ return True
  where
	copiesplural 1 = "copy"
	copiesplural _ = "copies"
	untrustedheader = "The following untrusted locations may also have copies:\n"
	ppwhereis h ls urls = do
		descm <- uuidDescriptions
		let urlvals = map (\(u, us) -> (u, Just (V.fromList us))) $
			filter (\(u,_) -> u `elem` ls) urls
		prettyPrintUUIDsWith (Just "urls") h descm (const Nothing) urlvals

getUUIDUrls :: Key -> [UUID] -> M.Map UUID Remote -> Annex [(UUID, [URLString])]
getUUIDUrls key uuids remotemap = forM uuids $ \uu -> (,)
	<$> pure uu
	<*> maybe (pure []) (getRemoteUrls key) (M.lookup uu remotemap)

getRemoteUrls :: Key -> Remote -> Annex [URLString]
getRemoteUrls key remote
	| uuid remote == webUUID = getWebUrls key
	| otherwise = (++)
		<$> askremote
		<*> claimedurls
  where
	askremote = case whereisKey remote of
		Nothing -> pure []
		Just w -> tryNonAsync (w key) >>= \case
			Right l -> pure l
			Left e -> do
				warning $ unwords
					[ "unable to query remote"
					, name remote
					, "for urls:"
					, show e
					]
				return []
	claimedurls = do
		us <- map fst 
			. filter (\(_, d) -> d == OtherDownloader)
			. map getDownloader
			<$> getUrls key
		filterM (\u -> (==) <$> pure remote <*> claimingUrl u) us

showRemoteUrls :: M.Map UUID Remote -> (UUID, [URLString]) -> Annex ()
showRemoteUrls remotemap (uu, us)
	| null us = noop
	| otherwise = case M.lookup uu remotemap of
		Just r -> showLongNote $ 
			unlines $ map (\u -> name r ++ ": " ++ u) us 
		Nothing -> noop
