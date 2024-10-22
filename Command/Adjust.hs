{- git-annex command
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Adjust where

import Command
import Annex.AdjustedBranch

cmd :: Command
cmd = notBareRepo $ noDaemonRunning $
	command "adjust" SectionSetup "enter adjusted branch"
		paramNothing (seek <$$> optParser)

optParser :: CmdParamsDesc -> Parser Adjustment
optParser _ =
	linkPresentAdjustmentParser
	<|> (LockUnlockPresentAdjustment <$> lockUnlockPresentAdjustmentParser)
	
linkPresentAdjustmentParser :: Parser Adjustment
linkPresentAdjustmentParser = comb <$> some ps
  where
	ps = (LinkAdjustment <$> linkAdjustmentParser)
		<|> (PresenceAdjustment <$> presenceAdjustmentParser <*> pure Nothing)
	comb (LinkAdjustment _ : LinkAdjustment b : c) =
		comb (LinkAdjustment b : c)
	comb (PresenceAdjustment _a1 a2 : PresenceAdjustment b1 b2 : c) = 
		comb (PresenceAdjustment b1 (b2 <|> a2) : c)
	comb (LinkAdjustment a : PresenceAdjustment b1 b2 : c) =
		comb (PresenceAdjustment b1 (b2 <|> Just a) : c)
	comb (PresenceAdjustment a1 _a2 : LinkAdjustment b : c) =
		comb (PresenceAdjustment a1 (Just b) : c)
	comb (a : _) = a
	comb [] = error "internal"

linkAdjustmentParser :: Parser LinkAdjustment
linkAdjustmentParser =
	flag' UnlockAdjustment
		( long "unlock"
		<> help "unlock annexed files"
		)
	<|> flag' LockAdjustment
		( long "lock"
		<> help "lock annexed files"
		)
	<|> flag' FixAdjustment
		( long "fix"
		<> help "fix symlinks to annnexed files"
		)

presenceAdjustmentParser :: Parser PresenceAdjustment
presenceAdjustmentParser =
	flag' HideMissingAdjustment
		( long "hide-missing"
		<> help "hide annexed files whose content is not present"
		)

lockUnlockPresentAdjustmentParser :: Parser LockUnlockPresentAdjustment
lockUnlockPresentAdjustmentParser =
	flag' UnlockPresentAdjustment
		( long "unlock-present"
		<> help "unlock files whose content is present; lock rest"
		)

seek :: Adjustment -> CommandSeek
seek = commandAction . start

start :: Adjustment -> CommandStart
start adj = do
	checkVersionSupported
	starting "adjust" (ActionItemOther Nothing) (SeekInput []) $
		next $ enterAdjustedBranch adj
