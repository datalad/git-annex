{- git-annex command
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Recompute where

import Command
import qualified Remote.Compute
import qualified Remote
import qualified Types.Remote as Remote
import qualified Git.Ref as Git
import Annex.Content
import Annex.CatFile
import Annex.GitShaKey
import Git.FilePath
import Logs.Location
import Command.AddComputed (Reproducible(..), parseReproducible, getInputContent, getInputContent', addComputed)
import Backend (maybeLookupBackendVariety, unknownBackendVarietyMessage)
import Types.Key

import qualified Data.Map as M

cmd :: Command
cmd = notBareRepo $ 
	command "recompute" SectionCommon "recompute computed files"
		paramPaths (seek <$$> optParser)

data RecomputeOptions = RecomputeOptions
	{ recomputeThese :: CmdParams
	, originalOption :: Bool
	, reproducible :: Maybe Reproducible
	, computeRemote :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser RecomputeOptions
optParser desc = RecomputeOptions
	<$> cmdParams desc
	<*> switch
		( long "original"
		<> help "recompute using original content of input files"
		)
	<*> parseReproducible
	<*> optional (mkParseRemoteOption <$> parseRemoteOption)

seek :: RecomputeOptions -> CommandSeek
seek o = startConcurrency commandStages (seek' o)

seek' :: RecomputeOptions -> CommandSeek
seek' o = do
	computeremote <- maybe (pure Nothing) (Just <$$> getParsed)
		(computeRemote o)
	let seeker = AnnexedFileSeeker
		{ startAction = const $ start o computeremote
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	withFilesInGitAnnex ww seeker
		=<< workTreeItems ww (recomputeThese o)
  where
	ww = WarnUnmatchLsFiles "recompute"

start :: RecomputeOptions -> Maybe Remote -> SeekInput -> OsPath -> Key -> CommandStart
start o (Just computeremote) si file key = 
	stopUnless (elem (Remote.uuid computeremote) <$> loggedLocations key) $
		start' o computeremote si file key		
start o Nothing si file key = do
	rs <- catMaybes <$> (mapM Remote.byUUID =<< loggedLocations key)
	case sortOn Remote.cost $ filter Remote.Compute.isComputeRemote rs of
		[] -> stop
		(r:_) -> start' o r si file key

start' :: RecomputeOptions -> Remote -> SeekInput -> OsPath -> Key -> CommandStart
start' o r si file key =
	Remote.Compute.getComputeState
		(Remote.remoteStateHandle r) key >>= \case
			Nothing -> stop
			Just state -> shouldrecompute state >>= \case
				Nothing -> stop
				Just mreason -> starting "recompute" ai si $ do
					maybe noop showNote mreason
					perform o r file key state
  where
	ai = mkActionItem (key, file)

	shouldrecompute state
		| originalOption o = return (Just Nothing)
		| otherwise = firstM (inputchanged state)
			(M.toList (Remote.Compute.computeInputs state))
			>>= return . \case
				Nothing -> Nothing
				Just (inputfile, _) -> Just $ Just $
					QuotedPath inputfile <> " changed"

	inputchanged state (inputfile, inputkey) = do
		-- Note that the paths from the remote state are not to be
		-- trusted to point to a file in the repository, but using
		-- the path with git cat-file will only succeed if it
		-- is checked into the repository.
		p <- fromRepo $ fromTopFilePath $ asTopFilePath $
			Remote.Compute.computeSubdir state </> inputfile
		case keyGitSha inputkey of
			Nothing -> 
				catKeyFile p >>= return . \case
					Just k -> k /= inputkey
					Nothing -> inputfilemissing
			Just inputgitsha -> inRepo (Git.fileRef p) >>= \case
				Just fileref -> catObjectMetaData fileref >>= return . \case
					Just (sha, _, _) -> sha /= inputgitsha
					Nothing -> inputfilemissing
				Nothing -> return inputfilemissing
	  where
		-- When an input file is missing, go ahead and recompute.
		-- This way, the user will see the computation fail,
		-- with an error message that explains the problem.
		-- Or, if the input file is only optionally used by the
		-- computation, it might succeed.
		inputfilemissing = True

perform :: RecomputeOptions -> Remote -> OsPath -> Key -> Remote.Compute.ComputeState -> CommandPerform
perform o r file origkey origstate = do
	program <- Remote.Compute.getComputeProgram r
	reproducibleconfig <- getreproducibleconfig
	showOutput
	Remote.Compute.runComputeProgram program origstate
		(Remote.Compute.ImmutableState False)
		(getinputcontent program)
		(go program reproducibleconfig)
	next $ return True
  where
	go program reproducibleconfig state tmpdir ts = do
		checkbehaviorchange program state
		addComputed "processing" False r reproducibleconfig
			choosebackend destfile False state tmpdir ts

	checkbehaviorchange program state = do
		let check s w a b = forM_ (M.keys (w a)) $ \f ->
			unless (M.member f (w b)) $
				Remote.Compute.computationBehaviorChangeError program s f
		
		check "not using input file"
			Remote.Compute.computeInputs origstate state
		check "outputting"
			Remote.Compute.computeOutputs state origstate
		check "not outputting"
			Remote.Compute.computeOutputs origstate state

	getinputcontent program p
		| originalOption o =
			case M.lookup p (Remote.Compute.computeInputs origstate) of
				Just inputkey -> getInputContent' False inputkey
					(fromOsPath p ++ "(key " ++ serializeKey inputkey ++ ")")
				Nothing -> Remote.Compute.computationBehaviorChangeError program
					"requesting a new input file" p
		| otherwise = getInputContent False p
	
	destfile outputfile
		| Just outputfile == origfile = Just file
		| otherwise = Nothing
	
	origfile = headMaybe $ M.keys $ M.filter (== Just origkey)
		(Remote.Compute.computeOutputs origstate)

	origbackendvariety = fromKey keyVariety origkey
	
	getreproducibleconfig = case reproducible o of
		Just (Reproducible True) -> return (Just (Reproducible True))
		-- A VURL key is used when the computation was
		-- unreproducible. So recomputing should too, but that
		-- will result in the same VURL key. Since moveAnnex
		-- will prefer the current annex object to a new one,
		-- delete the annex object first, so that if recomputing
		-- generates a new version of the file, it replaces
		-- the old version.
		v -> case origbackendvariety of
			VURLKey -> do
				lockContentForRemoval origkey noop removeAnnex
				-- in case computation fails or is interupted
				logStatus NoLiveUpdate origkey InfoMissing
				return (Just (Reproducible False))
			_ -> return v

	choosebackend _outputfile
		-- Use the same backend as was used to compute it before,
		-- so if the computed file is the same, there will be
		-- no change.
		| otherwise = maybeLookupBackendVariety origbackendvariety >>= \case
			Just b -> return b
			Nothing -> giveup $ unknownBackendVarietyMessage origbackendvariety
