{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.FilterBranch where

import Command

cmd :: Command
cmd = withGlobalOptions [annexedMatchingOptions] $ 
	command "filter-branch" SectionMaintenance 
		"filter information from the git-annex branch"
		paramPaths (seek <$$> optParser)

data FilterBranchOptions = FilterBranchOptions
	{ includeFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, includeKeyInformationFor :: [DeferredParse UUID]
	, excludeKeyInformationFor :: [DeferredParse UUID]
	, includeAllKeyInformation :: Bool
	, includeRepoConfigFor :: [DeferredParse UUID]
	, excludeRepoConfigFor :: [DeferredParse UUID]
	, includeAllRemoteConfig :: Bool
	, includeGlobalConfig :: Bool
	, excludeGlobalConfig :: Bool
	}

optParser :: CmdParamsDesc -> Parser FilterBranchOptions
optParser desc = FilterBranchOptions
	<$> cmdParams desc
	<*> optional parseKeyOptions
	<*> many
		( parseRepositoryOption "include-key-information-for"
			"include key information for a repository"
		)
	<*> many
		( parseRepositoryOption "exclude-key-information-for"
			"exclude key information for a repository"
		)
	<*> switch
		( long "include-all-key-information"
		<> help "include key information for all repositories"
		)
	<*> many
		( parseRepositoryOption "include-repo-config-for"
			"include configuration specific to a repository"
		)
	<*> many
		( parseRepositoryOption "exclude-repo-config-for"
			"exclude configuration specific to a repository"
		)
	<*> switch
		( long "include-all-repo-config"
		<> help "include configuration of all repositories"
		)
	<*> switch
		( long "include-global-config"
		<> help "include global configuration"
		)
	<*> switch
		( long "exclude-global-config"
		<> help "exclude global configuration"
		)

parseRepositoryOption :: String -> String -> Parser (DeferredParse UUID)
parseRepositoryOption s h = parseUUIDOption <$> strOption
	( long s
	<> metavar (paramRemote `paramOr` paramDesc `paramOr` paramUUID)
	<> help h
	<> completeRemotes
	)

seek :: FilterBranchOptions -> CommandSeek
seek o = do
	error "TODO"
