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
	(LinkAdjustment <$> linkAdjustmentParser)
	<|> (PresenceAdjustment <$> presenceAdjustmentParser <*> maybeLinkAdjustmentParser)
	<|> (LinkPresentAdjustment <$> linkPresentAdjustmentParser)

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

maybeLinkAdjustmentParser :: Parser (Maybe LinkAdjustment)
maybeLinkAdjustmentParser = Just <$> linkAdjustmentParser <|> pure Nothing

presenceAdjustmentParser :: Parser PresenceAdjustment
presenceAdjustmentParser =
	flag' HideMissingAdjustment
		( long "hide-missing"
		<> help "hide annexed files whose content is not present"
		)

linkPresentAdjustmentParser :: Parser LinkPresentAdjustment
linkPresentAdjustmentParser =
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
