{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.FilterBranch where

import Command

import qualified Data.Set as S

cmd :: Command
cmd = withGlobalOptions [annexedMatchingOptions] $ 
	command "filter-branch" SectionMaintenance 
		"filter information from the git-annex branch"
		paramPaths (seek <$$> optParser)

data FilterBranchOptions = FilterBranchOptions
	{ includeFiles :: CmdParams
	, keyOptions :: Maybe KeyOptions
	, keyInformation :: [IncludeExclude (DeferredParse UUID)]
	, repoConfig :: [IncludeExclude (DeferredParse UUID)]
	, includeGlobalConfig :: Bool
	}

optParser :: CmdParamsDesc -> Parser FilterBranchOptions
optParser desc = FilterBranchOptions
	<$> cmdParams desc
	<*> optional parseKeyOptions
	<*> many (parseIncludeExclude "key-information")
	<*> many (parseIncludeExclude "repo-config")
	<*> switch
		( long "include-global-config"
		<> help "include global configuration"
		)

data IncludeExclude t
	= Include t
	| Exclude t
	| IncludeAll
	deriving (Show, Eq, Ord)

isInclude :: IncludeExclude t -> Bool
isInclude (Include _) = True
isInclude IncludeAll = True
isInclude (Exclude _) = False

parseIncludeExclude :: String -> Parser (IncludeExclude (DeferredParse UUID))
parseIncludeExclude s = 
	( Include <$> parseRepositoryOption
		("include-" ++ s ++ "-for")
		"include information about a repository"
	) <|>
	( Exclude <$> parseRepositoryOption
		("exclude-" ++ s ++ "-for")
		"exclude information about a repository"
	) <|>
	( flag' IncludeAll 
		( long ("include-all-" ++ s)
		<> help "include information about all non-excluded repositories"
		)
	)

parseRepositoryOption :: String -> String -> Parser (DeferredParse UUID)
parseRepositoryOption s h = parseUUIDOption <$> strOption
	( long s
	<> help h
	<> metavar (paramRemote `paramOr` paramDesc `paramOr` paramUUID)
	<> completeRemotes
	)

mkUUIDMatcher :: [IncludeExclude (DeferredParse UUID)] -> Annex (UUID -> Bool)
mkUUIDMatcher l = mkUUIDMatcher' <$> mapM get l
  where
	get (Include v) = Include <$> getParsed v
	get (Exclude v) = Exclude <$> getParsed v
	get IncludeAll = pure IncludeAll

mkUUIDMatcher' :: [IncludeExclude UUID] -> (UUID -> Bool)
mkUUIDMatcher' l = \u -> 
	(S.member (Include u) includes || S.member IncludeAll includes)
		&& S.notMember (Exclude u) excludes
  where
	(includes, excludes) = S.partition isInclude (S.fromList l)

seek :: FilterBranchOptions -> CommandSeek
seek o = do
	keyinfomatcher <- mkUUIDMatcher (keyInformation o)
	configinfomatcher <- mkUUIDMatcher (repoConfig o)
	error "TODO"
