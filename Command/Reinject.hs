{- git-annex command
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
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
cmd = withAnnexOptions [backendOption] $
	command "reinject" SectionUtility 
		"inject content of file back into annex"
		(paramRepeating (paramPair "SRC" "DEST"))
		(seek <$$> optParser)

data ReinjectOptions = ReinjectOptions
	{ params :: CmdParams
	, knownOpt :: Bool
	}

optParser :: CmdParamsDesc -> Parser ReinjectOptions
optParser desc = ReinjectOptions
	<$> cmdParams desc
	<*> switch
		( long "known"
		<> help "inject all known files"
		<> hidden
		)

seek :: ReinjectOptions -> CommandSeek
seek os
	| knownOpt os = withStrings (commandAction . startKnown) (params os)
	| otherwise = withWords (commandAction . startSrcDest) (params os)

startSrcDest :: [FilePath] -> CommandStart
startSrcDest ps@(src:dest:[])
	| src == dest = stop
	| otherwise = notAnnexed src' $
		lookupKey (toRawFilePath dest) >>= \case
			Just k -> go k
			Nothing -> do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $ QuotedPath src'
					<> " is not an annexed file"
  where
	src' = toRawFilePath src
	go key = starting "reinject" ai si $
		ifM (verifyKeyContent key src')
			( perform src' key
			, do
				qp <- coreQuotePath <$> Annex.getGitConfig
				giveup $ decodeBS $ quote qp $ QuotedPath src'
					<> " does not have expected content of "
					<> QuotedPath (toRawFilePath dest)
			)
	ai = ActionItemOther (Just (QuotedPath src'))
	si = SeekInput ps
startSrcDest _ = giveup "specify a src file and a dest file"

startKnown :: FilePath -> CommandStart
startKnown src = notAnnexed src' $
	starting "reinject" ai si $ do
		(key, _) <- genKey ks nullMeterUpdate =<< defaultBackend
		ifM (isKnownKey key)
			( perform src' key
			, do
				warning "Not known content; skipping"
				next $ return True
			)
  where
	src' = toRawFilePath src
	ks = KeySource src' src' Nothing
	ai = ActionItemOther (Just (QuotedPath src'))
	si = SeekInput [src]

notAnnexed :: RawFilePath -> CommandStart -> CommandStart
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

perform :: RawFilePath -> Key -> CommandPerform
perform src key = ifM move
	( next $ cleanup key
	, giveup "failed"
	)
  where
	move = checkDiskSpaceToGet key False $
		moveAnnex key (AssociatedFile Nothing) src

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
