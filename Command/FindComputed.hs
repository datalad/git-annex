{- git-annex command
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Command.FindComputed where

import Command
import Git.FilePath
import qualified Utility.Format
import Utility.Terminal
import Command.Find (showFormatted, formatVars)
import Remote.Compute (isComputeRemote, getComputeState, ComputeState(..))
import qualified Remote
import qualified Types.Remote as Remote
import Database.Keys
import Annex.CatFile

import qualified Data.Map as M

cmd :: Command
cmd = withAnnexOptions [annexedMatchingOptions] $ noCommit $ noMessages $
	withAnnexOptions [jsonOptions] $
		command "findcomputed" SectionQuery "lists computed files"
			paramPaths (seek <$$> optParser)

data FindComputedOptions = FindComputedOptions
	{ findThese :: CmdParams
	, formatOption :: Maybe Utility.Format.Format
	, keyOptions :: Maybe KeyOptions
	, inputsOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser FindComputedOptions
optParser desc = FindComputedOptions
	<$> cmdParams desc
	<*> optional parseFormatOption
	<*> optional parseBranchKeysOption
	<*> switch
                ( long "inputs"
                <> help "display input files"
                )
	
parseFormatOption :: Parser Utility.Format.Format
parseFormatOption = 
	option (Utility.Format.gen <$> str)
		( long "format" <> metavar paramFormat
		<> help "control format of output"
		)

seek :: FindComputedOptions -> CommandSeek
seek o = do
	unless (isJust (keyOptions o)) $
		checkNotBareRepo
	isterminal <- liftIO $ checkIsTerminal stdout
	computeremotes <- filter isComputeRemote <$> Remote.remoteList
	let seeker = AnnexedFileSeeker
		{ startAction = const (start o isterminal computeremotes)
		, checkContentPresent = Nothing
		, usesLocationLog = True
		}
	withKeyOptions (keyOptions o) False seeker
		(commandAction . startKeys o isterminal computeremotes)
		(withFilesInGitAnnex ww seeker)
		=<< workTreeItems ww (findThese o)
  where
	ww = WarnUnmatchLsFiles "findcomputed"

start :: FindComputedOptions -> IsTerminal -> [Remote] -> SeekInput -> OsPath -> Key -> CommandStart
start o isterminal computeremotes _ file key = do
	rs <- Remote.remotesWithUUID computeremotes
		<$> Remote.keyLocations key
	rcs <- catMaybes <$> forM rs get
	if null rcs
		then stop
		else startingCustomOutput key $ do
			forM_ rcs display
			next $ return True
  where
	get r = fmap (r, )
		<$> getComputeState (Remote.remoteStateHandle r) key
	
	showformatted = showFormatted isterminal (formatOption o)
				
	unformatted r computation = fromOsPath file 
		<> " (" <> encodeBS (Remote.name r)
		<> ") -- "
		<> encodeBS computation
	
	unformattedinputs (Right inputfile) = fromOsPath file
		<> " " <> fromOsPath inputfile
	unformattedinputs (Left inputkey) = fromOsPath file
		<> " " <> serializeKey' inputkey
	
	display (r, c) = do
		let computation = unwords (computeParams c)
		let formatvars = 
			[ ("remote", Remote.name r)
			, ("computation", computation)
			] ++ formatVars key (AssociatedFile (Just file))
		if inputsOption o
			then forM_ (M.elems $ computeInputs c) $ \inputkey -> do
				input <- maybe (Left inputkey) Right
					<$> getassociated inputkey
				showformatted (unformattedinputs input) $
					[ ("input", either serializeKey fromOsPath input)
					, ("inputkey", serializeKey inputkey)
					, ("inputfile", either (const "") fromOsPath input)
					] ++ formatvars
			else showformatted (unformatted r computation) formatvars

	getassociated inputkey = 
		getAssociatedFiles inputkey
			>>= mapM (fromRepo . fromTopFilePath)
			>>= firstM (stillassociated inputkey)

	-- Some associated files that are in the keys database may no
	-- longer correspond to files in the repository.
	stillassociated k f = catKeyFile f >>= return . \case
		Just k' | k' == k -> True
		_ -> False

startKeys :: FindComputedOptions -> IsTerminal -> [Remote] -> (SeekInput, Key, ActionItem) -> CommandStart
startKeys o isterminal computeremotes (si, key, ActionItemBranchFilePath (BranchFilePath _ topf) _) =
	start o isterminal computeremotes si (getTopFilePath topf) key
startKeys _ _ _ _ = stop

