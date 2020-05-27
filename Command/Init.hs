{- git-annex command
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Init where

import Command
import Annex.Init
import Annex.Version
import Types.RepoVersion
import qualified Annex.SpecialRemote

import Control.Monad.Fail as Fail (MonadFail(..))
import qualified Data.Map as M
	
cmd :: Command
cmd = dontCheck repoExists $
	command "init" SectionSetup "initialize git-annex"
		paramDesc (seek <$$> optParser)

data InitOptions = InitOptions
	{ initDesc :: String
	, initVersion :: Maybe RepoVersion
	, autoEnableOnly :: Bool
	}

optParser :: CmdParamsDesc -> Parser InitOptions
optParser desc = InitOptions
	<$> (unwords <$> cmdParams desc)
	<*> optional (option (str >>= parseRepoVersion)
		( long "version" <> metavar paramValue
		<> help "Override default annex.version"
		))
	<*> switch
		( long "autoenable"
		<> help "only enable special remotes configured with autoenable=true"
		)

parseRepoVersion :: MonadFail m => String -> m RepoVersion
parseRepoVersion s = case RepoVersion <$> readish s of
	Nothing -> Fail.fail $ "version parse error"
	Just v
		| v `elem` supportedVersions -> return v
		| otherwise -> case M.lookup v autoUpgradeableVersions of
			Just v' -> return v'
			Nothing -> Fail.fail $ s ++ " is not a currently supported repository version"

seek :: InitOptions -> CommandSeek
seek = commandAction . start

start :: InitOptions -> CommandStart
start os
	| autoEnableOnly os = starting "init" (ActionItemOther (Just "autoenable")) $
		performAutoEnableOnly
	| otherwise = starting "init" (ActionItemOther (Just $ initDesc os)) $
		perform os

perform :: InitOptions -> CommandPerform
perform os = do
	case initVersion os of
		Nothing -> noop
		Just wantversion -> getVersion >>= \case
			Just v | v /= wantversion ->
				giveup $ "This repository is already a initialized with version " ++ show (fromRepoVersion v) ++ ", not changing to requested version."
			_ -> noop
	initialize
		(if null (initDesc os) then Nothing else Just (initDesc os))
		(initVersion os)
	Annex.SpecialRemote.autoEnable
	next $ return True

performAutoEnableOnly :: CommandPerform
performAutoEnableOnly = do
	Annex.SpecialRemote.autoEnable
	next $ return True
