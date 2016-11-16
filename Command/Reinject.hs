{- git-annex command
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinject where

import Command
import Logs.Location
import Annex.Content
import Backend
import Types.KeySource

cmd :: Command
cmd = command "reinject" SectionUtility 
	"inject content of file back into annex"
	(paramRepeating (paramPair "SRC" "DEST")
		`paramOr` "--known " ++ paramRepeating "SRC")
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
	| knownOpt os = withStrings startKnown (params os)
	| otherwise = withWords startSrcDest (params os)

startSrcDest :: [FilePath] -> CommandStart
startSrcDest (src:dest:[])
	| src == dest = stop
	| otherwise = notAnnexed src $ do
		showStart "reinject" dest
		next $ ifAnnexed dest
			(\key -> perform src key (verifyKeyContent DefaultVerify UnVerified key src))
			stop
startSrcDest _ = error "specify a src file and a dest file"

startKnown :: FilePath -> CommandStart
startKnown src = notAnnexed src $ do
	showStart "reinject" src
	mkb <- genKey (KeySource src src Nothing) Nothing
	case mkb of
		Nothing -> error "Failed to generate key"
		Just (key, _) -> ifM (isKnownKey key)
			( next $ perform src key (return True)
			, do
				warning "Not known content; skipping"
				next $ next $ return True
			)

notAnnexed :: FilePath -> CommandStart -> CommandStart
notAnnexed src = ifAnnexed src (error $ "cannot used annexed file as src: " ++ src)

perform :: FilePath -> Key -> Annex Bool -> CommandPerform
perform src key verify = ifM move
	( next $ cleanup key
	, error "failed"
	)
  where
	move = checkDiskSpaceToGet key False $
		ifM verify
			( do
				moveAnnex key src
				return True
			, return False
			)

cleanup :: Key -> CommandCleanup
cleanup key = do
	logStatus key InfoPresent
	return True
