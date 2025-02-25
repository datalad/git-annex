{- git-annex command
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.AddComputed where

import Command
import qualified Git
import qualified Annex
import qualified Remote.Compute
import qualified Types.Remote as Remote
import Annex.CatFile
import Annex.Content.Presence
import Annex.Ingest
import Types.RemoteConfig
import Types.KeySource
import Messages.Progress
import Utility.MonotonicClock
import Logs.Location

import qualified Data.Map as M
import Data.Time.Clock

cmd :: Command
cmd = notBareRepo $ 
	command "addcomputed" SectionCommon "add computed files to annex"
		(paramRepeating paramExpression)
		(seek <$$> optParser)

data AddComputedOptions = AddComputedOptions
	{ computeParams :: CmdParams
	, computeRemote :: DeferredParse Remote
	, reproducible :: Reproducible
	}

optParser :: CmdParamsDesc -> Parser AddComputedOptions
optParser desc = AddComputedOptions
	<$> cmdParams desc
	<*> (mkParseRemoteOption <$> parseToOption)
	<*> (fromMaybe Unreproducible <$> parseReproducible)

data Reproducible = Reproducible | Unreproducible

parseReproducible :: Parser (Maybe Reproducible)
parseReproducible = r <|> unr
  where
	r  = flag Nothing (Just Reproducible)
		( long "reproducible"
		<> short 'r'
		<> help "computation is fully reproducible"
		)
	unr = flag Nothing (Just Unreproducible)
		( long "unreproducible"
		<> short 'u'
		<> help "computation is not fully reproducible"
		)

seek :: AddComputedOptions -> CommandSeek
seek o = startConcurrency commandStages (seek' o)

seek' :: AddComputedOptions -> CommandSeek
seek' o = do
	r <- getParsed (computeRemote o)
	unless (Remote.typename (Remote.remotetype r) == Remote.typename Remote.Compute.remote) $
		giveup "That is not a compute remote."

	let rc = unparsedRemoteConfig (Remote.config r)
	case Remote.Compute.getComputeProgram rc of
		Left err -> giveup $ 
			"Problem with the configuration of the compute remote: " ++ err
		Right program -> commandAction $ start o r program

start :: AddComputedOptions -> Remote -> Remote.Compute.ComputeProgram -> CommandStart
start o r program = starting "addcomputed" ai si $ perform o r program
  where
	ai = ActionItemUUID (Remote.uuid r) (UnquotedString (Remote.name r))
	si = SeekInput (computeParams o)

perform :: AddComputedOptions -> Remote -> Remote.Compute.ComputeProgram -> CommandPerform
perform o r program = do
	repopath <- fromRepo Git.repoPath
	subdir <- liftIO $ relPathDirToFile repopath (literalOsPath ".")
	let state = Remote.Compute.ComputeState
		{ Remote.Compute.computeParams = computeParams o
		, Remote.Compute.computeInputs = mempty
		, Remote.Compute.computeOutputs = mempty
		, Remote.Compute.computeSubdir = subdir
		, Remote.Compute.computeReproducible = 
			case reproducible o of
				Reproducible -> True
				Unreproducible -> False
		}
	fast <- Annex.getRead Annex.fast
	starttime <- liftIO currentMonotonicTimestamp
	Remote.Compute.runComputeProgram program state
		(Remote.Compute.ImmutableState False)
		(getinputcontent fast)
		(go starttime)
	next $ return True
  where
	getinputcontent fast p = catKeyFile p >>= \case
		Just inputkey -> do
			obj <- calcRepo (gitAnnexLocation inputkey)
			if fast
				then return (inputkey, Nothing)
				else ifM (inAnnex inputkey)
					( return (inputkey, Just obj)
					, giveup $ "The computation needs the content of a file which is not present: " ++ fromOsPath p
					)
		Nothing -> ifM (liftIO $ doesFileExist p)
			( giveup $ "The computation needs an input file that is not an annexed file: " ++ fromOsPath p 
			, giveup $ "The computation needs an input file which does not exist: " ++ fromOsPath p
			)
	
	go starttime state tmpdir = do
		endtime <- liftIO currentMonotonicTimestamp
		let ts = calcduration starttime endtime
		let outputs = Remote.Compute.computeOutputs state
		when (M.null outputs) $
			giveup "The computation succeeded, but it did not generate any files."
		oks <- forM (M.keys outputs) $ \outputfile -> do
			showAction $ "adding " <> QuotedPath outputfile
			k <- catchNonAsync (addfile tmpdir outputfile)
				(\err -> giveup $ "Failed to ingest output file " ++ fromOsPath outputfile ++ ": " ++ show err)
			return (outputfile, Just k)
		let state' = state
			{ Remote.Compute.computeOutputs = M.fromList oks
			}
		forM_ (mapMaybe snd oks) $ \k -> do
			Remote.Compute.setComputeState
				(Remote.remoteStateHandle r)
				k ts state'
			logChange NoLiveUpdate k (Remote.uuid r) InfoPresent
	
	addfile tmpdir outputfile = do
		let outputfile' = tmpdir </> outputfile
		let ld = LockedDown ldc $ KeySource
			{ keyFilename = outputfile
			, contentLocation = outputfile'
			, inodeCache = Nothing
			}
		sz <- liftIO $ getFileSize outputfile'
		metered Nothing sz Nothing $ \_ p ->
			ingestAdd p (Just ld) >>= \case
				Nothing -> giveup "key generation failed"
				Just k -> return k

	ldc = LockDownConfig
		{ lockingFile = True
		, hardlinkFileTmpDir = Nothing
		, checkWritePerms = True
		}

	calcduration (MonotonicTimestamp starttime) (MonotonicTimestamp endtime) =
		fromIntegral (endtime - starttime) :: NominalDiffTime
