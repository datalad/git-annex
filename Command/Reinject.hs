{- git-annex command
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Reinject where

import Command
import Logs.Location
import Annex.Content
import Backend
import Types.KeySource
import Utility.Metered
import Annex.WorkTree
import qualified Git
import qualified Annex

cmd :: Command
cmd = withAnnexOptions [backendOption, jsonOptions] $
	command "reinject" SectionUtility 
		"inject content of file back into annex"
		(paramRepeating (paramPair "SRC" "DEST"))
		(seek <$$> optParser)

data ReinjectOptions = ReinjectOptions
	{ params :: CmdParams
	, knownOpt :: Bool
	, guessKeysOpt :: Bool
	}

optParser :: CmdParamsDesc -> Parser ReinjectOptions
optParser desc = ReinjectOptions
	<$> cmdParams desc
	<*> switch
		( long "known"
		<> help "inject all known files"
		<> hidden
		)
	<*> switch
		( long "guesskeys"
		<> help "inject files that are named like keys"
		<> hidden
		)

seek :: ReinjectOptions -> CommandSeek
seek os
	| guessKeysOpt os && knownOpt os = giveup "Cannot combine --known with --guesskeys"
	| guessKeysOpt os = withStrings (commandAction . startGuessKeys) (params os)
	| knownOpt os = withStrings (commandAction . startKnown) (params os)
	| otherwise = withPairs (commandAction . startSrcDest) (params os)

startSrcDest :: (SeekInput, (String, String)) -> CommandStart
startSrcDest (si, (src, dest))
	| src == dest = stop
	| otherwise = starting "reinject" ai si $ notAnnexed src' $
		lookupKey (toOsPath dest) >>= \case
			Just key -> ifM (verifyKeyContent key src')
				( perform src' key
				, do
					qp <- coreQuotePath <$> Annex.getGitConfig
					giveup $ decodeBS $ quote qp $ QuotedPath src'
						<> " does not have expected content of "
						<> QuotedPath (toOsPath dest)
				)
			Nothing -> do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $ QuotedPath src'
					<> " is not an annexed file"
  where
	src' = toOsPath src
	ai = ActionItemOther (Just (QuotedPath src'))

startGuessKeys :: FilePath -> CommandStart
startGuessKeys src = starting "reinject" ai si $ notAnnexed src' $
	case fileKey (takeFileName src') of
		Just key -> ifM (verifyKeyContent key src')
			( perform src' key
			, do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $ QuotedPath src'
					<> " does not have expected content"
			)
		Nothing -> do
			warning "Not named like an object file; skipping"
			next $ return True
  where
	src' = toOsPath src
	ai = ActionItemOther (Just (QuotedPath src'))
	si = SeekInput [src]

startKnown :: FilePath -> CommandStart
startKnown src = starting "reinject" ai si $ notAnnexed src' $ do
	(key, _) <- genKey ks nullMeterUpdate =<< defaultBackend
	ifM (isKnownKey key)
		( perform src' key
		, do
			warning "Not known content; skipping"
			next $ return True
		)
  where
	src' = toOsPath src
	ks = KeySource src' src' Nothing
	ai = ActionItemOther (Just (QuotedPath src'))
	si = SeekInput [src]

notAnnexed :: OsPath -> CommandPerform -> CommandPerform
notAnnexed src a = 
	ifM (fromRepo Git.repoIsLocalBare)
		( a
		, lookupKey src >>= \case
			Just _ -> do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $ 
					"cannot used annexed file as src: "
						<> QuotedPath src
			Nothing -> a
		)

perform :: OsPath -> Key -> CommandPerform
perform src key = do
	maybeAddJSONField "key" (serializeKey key)
	ifM move
		( next $ cleanup key
		, giveup "failed"
		)
  where
	move = checkDiskSpaceToGet key Nothing False $
		moveAnnex key (AssociatedFile Nothing) src

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus NoLiveUpdate key InfoPresent
	return True
