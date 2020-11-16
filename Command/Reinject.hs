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
startSrcDest ps@(src:dest:[])
	| src == dest = stop
	| otherwise = notAnnexed src' $
		ifAnnexed (toRawFilePath dest) go stop
  where
	src' = toRawFilePath src
	go key = starting "reinject" ai si $
		ifM (verifyKeyContent RetrievalAllKeysSecure DefaultVerify UnVerified key src')
			( perform src' key
			, giveup $ src ++ " does not have expected content of " ++ dest
			)
	ai = ActionItemOther (Just src)
	si = SeekInput ps
startSrcDest _ = giveup "specify a src file and a dest file"

startKnown :: FilePath -> CommandStart
startKnown src = notAnnexed src' $
	starting "reinject" ai si $ do
		(key, _) <- genKey ks nullMeterUpdate Nothing
		ifM (isKnownKey key)
			( perform src' key
			, do
				warning "Not known content; skipping"
				next $ return True
			)
  where
	src' = toRawFilePath src
	ks = KeySource src' src' Nothing
	ai = ActionItemOther (Just src)
	si = SeekInput [src]

notAnnexed :: RawFilePath -> CommandStart -> CommandStart
notAnnexed src a = 
	ifM (fromRepo Git.repoIsLocalBare)
		( a
		, ifAnnexed src
			(giveup $ "cannot used annexed file as src: " ++ fromRawFilePath src)
			a
		)

perform :: RawFilePath -> Key -> CommandPerform
perform src key = ifM move
	( next $ cleanup key
	, error "failed"
	)
  where
	move = checkDiskSpaceToGet key False $
		moveAnnex key (AssociatedFile Nothing) src

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
