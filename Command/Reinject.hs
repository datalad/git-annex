{- git-annex command
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Reinject where

import Command
import Logs.Location
import Annex.Content
import Backend
import Types.KeySource
import Utility.Metered
import qualified Git

cmd :: Command
cmd = command "reinject" SectionUtility 
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
startSrcDest (src:dest:[])
	| src == dest = stop
	| otherwise = notAnnexed src $ ifAnnexed (toRawFilePath dest) go stop
  where
	go key = starting "reinject" (ActionItemOther (Just src)) $
		ifM (verifyKeyContent RetrievalAllKeysSecure DefaultVerify UnVerified key src)
			( perform src key
			, giveup $ src ++ " does not have expected content of " ++ dest
			)
startSrcDest _ = giveup "specify a src file and a dest file"

startKnown :: FilePath -> CommandStart
startKnown src = notAnnexed src $
	starting "reinject" (ActionItemOther (Just src)) $ do
		mkb <- genKey ks nullMeterUpdate Nothing
		case mkb of
			Nothing -> error "Failed to generate key"
			Just (key, _) -> ifM (isKnownKey key)
				( perform src key
				, do
					warning "Not known content; skipping"
					next $ return True
				)
  where
	ks = KeySource src' src' Nothing
	src' = toRawFilePath src

notAnnexed :: FilePath -> CommandStart -> CommandStart
notAnnexed src a = 
	ifM (fromRepo Git.repoIsLocalBare)
		( a
		, ifAnnexed (toRawFilePath src)
			(giveup $ "cannot used annexed file as src: " ++ src)
			a
		)

perform :: FilePath -> Key -> CommandPerform
perform src key = ifM move
	( next $ cleanup key
	, error "failed"
	)
  where
	move = checkDiskSpaceToGet key False $
		moveAnnex key src

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
