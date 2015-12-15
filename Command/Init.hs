{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Common.Annex
import Command
import Annex.Init
import Annex.Version
import qualified Annex.SpecialRemote
	
cmd :: Command
cmd = dontCheck repoExists $
	command "init" SectionSetup "initialize git-annex"
		paramDesc (seek <$$> optParser)

data InitOptions = InitOptions
	{ initDesc :: String
	, initVersion :: Maybe Version
	}

optParser :: CmdParamsDesc -> Parser InitOptions
optParser desc = InitOptions
	<$> (unwords <$> cmdParams desc)
	<*> optional (option (str >>= parseVersion)
		( long "version" <> metavar paramValue
		<> help "Override default annex.version"
		))

parseVersion :: Monad m => String -> m Version
parseVersion v
	| v `elem` supportedVersions = return v
	| otherwise = fail $ v ++ " is not a currently supported repository version"

seek :: InitOptions -> CommandSeek
seek = commandAction . start

start :: InitOptions -> CommandStart
start os = do
	showStart "init" (initDesc os)
	next $ perform os

perform :: InitOptions -> CommandPerform
perform os = do
	initialize 
		(if null (initDesc os) then Nothing else Just (initDesc os))
		(initVersion os)
	Annex.SpecialRemote.autoEnable
	next $ return True
