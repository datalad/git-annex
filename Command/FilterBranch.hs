{- git-annex command
 -
 - Copyright 2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.FilterBranch where

import Command
import qualified Annex
import qualified Annex.Branch
import Annex.Branch.Transitions (filterBranch, FileTransition(..))
import Annex.HashObject
import Annex.Tmp
import Logs
import Git.Types
import Git.FilePath
import Git.Index
import Git.Env
import Git.UpdateIndex
import qualified Git.LsTree as LsTree
import Utility.RawFilePath

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import qualified System.FilePath.ByteString as P

cmd :: Command
cmd = noMessages $ withGlobalOptions [annexedMatchingOptions] $ 
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
seek o = withOtherTmp $ \tmpdir -> do
	let tmpindex = tmpdir P.</> "index"
	gc <- Annex.getGitConfig
	r' <- Annex.inRepo $ \r ->
		addGitEnv r indexEnv (fromRawFilePath tmpindex)
	withUpdateIndex r' $ \h -> do
		keyinfomatcher <- mkUUIDMatcher (keyInformation o)
		configmatcher <- mkUUIDMatcher (repoConfig o)
		
		let addtoindex f sha = liftIO $ streamUpdateIndex' h $
			pureStreamer $ L.fromStrict $ LsTree.formatLsTree $ LsTree.TreeItem
				{ LsTree.mode = fromTreeItemType TreeFile
				, LsTree.typeobj = fmtObjectType BlobObject
				, LsTree.sha = sha
				, LsTree.size = Nothing
				, LsTree.file = asTopFilePath f
				}
		
		let filterbanch matcher f c
			| L.null c = noop
			| otherwise = case filterBranch matcher gc f c of
				ChangeFile builder -> do
					let c' = toLazyByteString builder
					unless (L.null c') $
						addtoindex f =<< hashBlob c'
				-- This could perhaps be optimised by looking
				-- up the sha of the file in the branch.
				PreserveFile -> addtoindex f =<< hashBlob c

		let addkeyinfo k = startingCustomOutput k $ do
			forM_ (keyLogFiles gc k) $ \f ->
				filterbanch keyinfomatcher f
					=<< Annex.Branch.get f
			next (return True)

		let seeker = AnnexedFileSeeker
			{ startAction = \_ _ k -> addkeyinfo k
			, checkContentPresent = Nothing
			, usesLocationLog = True
			}
		-- Avoid the usual default of all files in the current
		-- directory and below, because this command is documented
		-- as only including the information it has explicitly been
		-- told to include.
		when (not (null (includeFiles o)) || isJust (keyOptions o)) $
			withKeyOptions (keyOptions o) False seeker
				(commandAction . \(_, k, _) -> addkeyinfo k)
				(withFilesInGitAnnex ww seeker)
				=<< workTreeItems ww (includeFiles o)
	
	-- TODO output commit
	liftIO $ removeWhenExistsWith removeLink tmpindex
  where
	ww = WarnUnmatchLsFiles
