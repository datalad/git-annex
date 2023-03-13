{- metadata based branch views
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Annex.View where

import Annex.Common
import Annex.View.ViewedFile
import Types.View
import Types.AdjustedBranch
import Types.MetaData
import Annex.MetaData
import qualified Annex
import qualified Annex.Branch
import qualified Git
import qualified Git.DiffTree as DiffTree
import qualified Git.Branch
import qualified Git.LsFiles
import qualified Git.LsTree
import qualified Git.Ref
import Git.CatFile
import Git.UpdateIndex
import Git.Sha
import Git.Types
import Git.FilePath
import Annex.WorkTree
import Annex.GitOverlay
import Annex.Link
import Annex.CatFile
import Annex.Concurrent
import Annex.Content.Presence
import Logs
import Logs.MetaData
import Logs.View
import Utility.Glob
import Types.Command
import CmdLine.Action
import qualified Utility.RawFilePath as R

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Map as M
import qualified System.FilePath.ByteString as P
import Control.Concurrent.Async
import "mtl" Control.Monad.Writer

{- Each visible ViewFilter in a view results in another level of
 - subdirectory nesting. When a file matches multiple ways, it will appear
 - in multiple subdirectories. This means there is a bit of an exponential
 - blowup with a single file appearing in a crazy number of places!
 -
 - Capping the view size to 5 is reasonable; why wants to dig
 - through 5+ levels of subdirectories to find anything?
 -}
viewTooLarge :: View -> Bool
viewTooLarge view = visibleViewSize view > 5

visibleViewSize :: View -> Int
visibleViewSize = length . filter viewVisible . viewComponents

{- Parses field=value, field!=value, field?=value, tag, !tag, and ?tag
 -
 - Note that the field may not be a legal metadata field name,
 - but it's let through anyway.
 - This is useful when matching on directory names with spaces,
 - which are not legal MetaFields.
 -}
parseViewParam :: ViewUnset -> String -> (MetaField, ViewFilter)
parseViewParam vu s = case separate (== '=') s of
	('!':tag, []) | not (null tag) ->
		( tagMetaField
		, mkExcludeValues tag
		)
	('?':tag, []) | not (null tag) ->
		( tagMetaField
		, mkFilterOrUnsetValues tag
		)
	(tag, []) ->
		( tagMetaField
		, mkFilterValues tag
		)
	(field, wanted)
		| end field == "!" ->
			( mkMetaFieldUnchecked (T.pack (beginning field))
			, mkExcludeValues wanted
			)
		| end field == "?" ->
			( mkMetaFieldUnchecked (T.pack (beginning field))
			, mkFilterOrUnsetValues wanted
			)
		| otherwise ->
			( mkMetaFieldUnchecked (T.pack field)
			, mkFilterValues wanted
			)
  where
	mkExcludeValues = ExcludeValues . S.singleton . toMetaValue . encodeBS
	mkFilterValues v
		| any (`elem` v) ['*', '?'] = FilterGlob v
		| otherwise = FilterValues $ S.singleton $ toMetaValue $ encodeBS v
	mkFilterOrUnsetValues v
		| any (`elem` v) ['*', '?'] = FilterGlobOrUnset v vu
		| otherwise = FilterValuesOrUnset (S.singleton $ toMetaValue $ encodeBS v) vu

data ViewChange = Unchanged | Narrowing | Widening
	deriving (Ord, Eq, Show)

{- Updates a view, adding new fields to filter on (Narrowing), 
 - or allowing new values in an existing field (Widening). -}
refineView :: View -> [(MetaField, ViewFilter)] -> (View, ViewChange)
refineView origview = checksize . calc Unchanged origview
  where
	calc c v [] = (v, c)
	calc c v ((f, vf):rest) =
		let (v', c') = refine v f vf
		in calc (max c c') v' rest

	refine view field vf
		| field `elem` map viewField (viewComponents view) =
			let (components', viewchanges) = runWriter $
				mapM (\c -> updateViewComponent c field vf) (viewComponents view)
			    viewchange = if field `elem` map viewField (viewComponents origview)
				then maximum viewchanges
				else Narrowing
			in (view { viewComponents = components' }, viewchange)
		| otherwise = 
			let component = mkViewComponent field vf
			    view' = view { viewComponents = component : viewComponents view }
			in (view', Narrowing)
	
	checksize r@(v, _)
		| viewTooLarge v = giveup $ "View is too large (" ++ show (visibleViewSize v) ++ " levels of subdirectories)"
		| otherwise = r

updateViewComponent :: ViewComponent -> MetaField -> ViewFilter -> Writer [ViewChange] ViewComponent
updateViewComponent c field vf
	| viewField c == field = do
		let (newvf, viewchange) = combineViewFilter (viewFilter c) vf
		tell [viewchange]
		return $ mkViewComponent field newvf
	| otherwise = return c

{- Adds an additional filter to a view. This can only result in narrowing
 - the view. Multivalued filters are added in non-visible form. -}
filterView :: View -> [(MetaField, ViewFilter)] -> View
filterView v vs = v { viewComponents = viewComponents f' ++ viewComponents v}
  where
	f = fst $ refineView (v {viewComponents = []}) vs
	f' = f { viewComponents = map toinvisible (viewComponents f) }
	toinvisible c = c { viewVisible = False }

{- Combine old and new ViewFilters, yielding a result that matches
 - either old+new, or only new. Which depends on the types of things
 - being combined.
 -}
combineViewFilter :: ViewFilter -> ViewFilter -> (ViewFilter, ViewChange)
combineViewFilter old@(FilterValues olds) (FilterValues news)
	| combined == old = (combined, Unchanged)
	| otherwise = (combined, Widening)
  where
	combined = FilterValues (S.union olds news)
combineViewFilter old@(ExcludeValues olds) (ExcludeValues news)
	| combined == old = (combined, Unchanged)
	| otherwise = (combined, Narrowing)
  where
	combined = ExcludeValues (S.union olds news)
{- If we have FilterValues and change to a FilterGlob,
 - it's always a widening change, because the glob could match other
 - values. OTOH, going the other way, it's a Narrowing change if the old
 - glob matches all the new FilterValues. -}
combineViewFilter (FilterValues _) newglob@(FilterGlob _) =
	(newglob, Widening)
combineViewFilter (FilterGlob oldglob) new@(FilterValues s)
	| all (matchGlob (compileGlob oldglob CaseInsensitive (GlobFilePath False)) . decodeBS . fromMetaValue) (S.toList s) = (new, Narrowing)
	| otherwise = (new, Widening)
{- With two globs, the old one is discarded, and the new one is used.
 - We can tell if that's a narrowing change by checking if the old
 - glob matches the new glob. For example, "*" matches "foo*",
 - so that's narrowing. While "f?o" does not match "f??", so that's
 - widening. -}
combineViewFilter (FilterGlob old) newglob@(FilterGlob new)
	| old == new = (newglob, Unchanged)
	| matchGlob (compileGlob old CaseInsensitive (GlobFilePath False)) new = (newglob, Narrowing)
	| otherwise = (newglob, Widening)
{- Combining FilterValuesOrUnset and FilterGlobOrUnset with FilterValues
 - and FilterGlob maintains the OrUnset if the second parameter has it,
 - and is otherwise the same as combining without OrUnset, except that
 - eliminating the OrUnset can be narrowing, and adding it can be widening. -}
combineViewFilter old@(FilterValuesOrUnset olds _) (FilterValuesOrUnset news newvu)
	| combined == old = (combined, Unchanged)
	| otherwise = (combined, Widening)
  where
	combined = FilterValuesOrUnset (S.union olds news) newvu
combineViewFilter (FilterValues olds) (FilterValuesOrUnset news vu) =
	(combined, Widening)
  where
	combined = FilterValuesOrUnset (S.union olds news) vu
combineViewFilter old@(FilterValuesOrUnset olds _) (FilterValues news)
	| combined == old = (combined, Narrowing)
	| otherwise = (combined, Widening)
  where
	combined = FilterValues (S.union olds news)
combineViewFilter (FilterValuesOrUnset _ _) newglob@(FilterGlob _) =
	(newglob, Widening)
combineViewFilter (FilterGlob _) new@(FilterValuesOrUnset _ _) =
	(new, Widening)
combineViewFilter (FilterValues _) newglob@(FilterGlobOrUnset _ _) =
	(newglob, Widening)
combineViewFilter (FilterValuesOrUnset _ _) newglob@(FilterGlobOrUnset _ _) =
	(newglob, Widening)
combineViewFilter (FilterGlobOrUnset oldglob _) new@(FilterValues _) =
	combineViewFilter (FilterGlob oldglob) new
combineViewFilter (FilterGlobOrUnset oldglob _) new@(FilterValuesOrUnset _ _) =
	let (_, viewchange) = combineViewFilter (FilterGlob oldglob) new
	in (new, viewchange)
combineViewFilter (FilterGlobOrUnset old _) newglob@(FilterGlobOrUnset new _)
	| old == new = (newglob, Unchanged)
	| matchGlob (compileGlob old CaseInsensitive (GlobFilePath False)) new = (newglob, Narrowing)
	| otherwise = (newglob, Widening)
combineViewFilter (FilterGlob _) newglob@(FilterGlobOrUnset _ _) =
	(newglob, Widening)
combineViewFilter (FilterGlobOrUnset _ _) newglob@(FilterGlob _) =
	(newglob, Narrowing)
{- There is not a way to filter a value and also apply an exclude. So:
 - When adding an exclude to a filter, use only the exclude.
 - When adding a filter to an exclude, use only the filter. -}
combineViewFilter (FilterGlob _) new@(ExcludeValues _) = (new, Narrowing)
combineViewFilter (ExcludeValues _) new@(FilterGlob _) = (new, Widening)
combineViewFilter (FilterValues _) new@(ExcludeValues _) = (new, Narrowing)
combineViewFilter (ExcludeValues _) new@(FilterValues _) = (new, Widening)
combineViewFilter (FilterValuesOrUnset _ _) new@(ExcludeValues _) = (new, Narrowing)
combineViewFilter (ExcludeValues _) new@(FilterValuesOrUnset _ _) = (new, Widening)
combineViewFilter (FilterGlobOrUnset _ _) new@(ExcludeValues _) = (new, Narrowing)
combineViewFilter (ExcludeValues _) new@(FilterGlobOrUnset _ _) = (new, Widening)

{- Generates views for a file from a branch, based on its metadata
 - and the filename used in the branch.
 -
 - Note that a file may appear multiple times in a view, when it
 - has multiple matching values for a MetaField used in the View.
 -
 - Of course if its MetaData does not match the View, it won't appear at
 - all.
 -
 - Note that for efficiency, it's useful to partially
 - evaluate this function with the view parameter and reuse
 - the result. The globs in the view will then be compiled and memoized.
 -}
viewedFiles :: View -> MkViewedFile -> FilePath -> MetaData -> [ViewedFile]
viewedFiles view = 
	let matchers = map viewComponentMatcher (viewComponents view)
	in \mkviewedfile file metadata ->
		let matches = map (\m -> m metadata) matchers
		in if any isNothing matches
			then []
			else 
				let paths = pathProduct $
					map (map toviewpath) (visible matches)
				in if null paths
					then [mkviewedfile file]
					else map (</> mkviewedfile file) paths
  where
	visible = map (fromJust . snd) .
		filter (viewVisible . fst) .
		zip (viewComponents view)
	
	toviewpath (MatchingMetaValue v) = toViewPath v
	toviewpath (MatchingUnset v) = toViewPath (toMetaValue (encodeBS v))

data MatchingValue = MatchingMetaValue MetaValue | MatchingUnset String

{- Checks if metadata matches a ViewComponent filter, and if so
 - returns the value, or values that match. Self-memoizing on ViewComponent. -}
viewComponentMatcher :: ViewComponent -> (MetaData -> Maybe [MatchingValue])
viewComponentMatcher viewcomponent = \metadata -> 
	matcher Nothing (viewFilter viewcomponent)
		(currentMetaDataValues metafield metadata)
  where
	metafield = viewField viewcomponent
	matcher matchunset (FilterValues s) = 
		\values -> setmatches matchunset $ S.intersection s values
	matcher matchunset (FilterGlob glob) =
		let cglob = compileGlob glob CaseInsensitive (GlobFilePath False)
		in \values -> setmatches matchunset $
			S.filter (matchGlob cglob . decodeBS . fromMetaValue) values
	matcher _ (ExcludeValues excludes) = 
		\values -> 
			if S.null (S.intersection values excludes)
				then Just []
				else Nothing
	matcher _ (FilterValuesOrUnset s (ViewUnset u)) =
		matcher (Just [MatchingUnset u]) (FilterValues s)
	matcher _ (FilterGlobOrUnset glob (ViewUnset u)) =
		matcher (Just [MatchingUnset u]) (FilterGlob glob)

	setmatches matchunset s
		| S.null s = matchunset 
		| otherwise = Just $
			map MatchingMetaValue (S.toList s)

-- This is '∕', a unicode character that displays the same as '/' but is
-- not it. It is encoded using the filesystem encoding, which allows it
-- to be used even when not in a unicode capable locale.
pseudoSlash :: String
pseudoSlash = "\56546\56456\56469"

-- And this is '╲' similarly.
pseudoBackslash :: String
pseudoBackslash = "\56546\56469\56498"

-- And this is '﹕' similarly.
pseudoColon :: String
pseudoColon = "\56559\56505\56469"

toViewPath :: MetaValue -> FilePath
toViewPath = escapepseudo [] . decodeBS . fromMetaValue
  where
	escapepseudo s ('/':cs) = escapepseudo (pseudoSlash:s) cs
	escapepseudo s ('\\':cs) = escapepseudo (pseudoBackslash:s) cs
	escapepseudo s (':':cs) = escapepseudo (pseudoColon:s) cs
	escapepseudo s ('%':cs) = escapepseudo ("%%":s) cs
	escapepseudo s (c1:c2:c3:cs)
		| [c1,c2,c3] == pseudoSlash = escapepseudo ("%":pseudoSlash:s) cs
		| [c1,c2,c3] == pseudoBackslash = escapepseudo ("%":pseudoBackslash:s) cs
		| [c1,c2,c3] == pseudoColon = escapepseudo ("%":pseudoColon:s) cs
		| otherwise = escapepseudo ([c1]:s) (c2:c3:cs)
	escapepseudo s (c:cs) = escapepseudo ([c]:s) cs
	escapepseudo s [] = concat (reverse s)

fromViewPath :: FilePath -> MetaValue
fromViewPath = toMetaValue . encodeBS . deescapepseudo []
  where
	deescapepseudo s ('%':escapedc:cs) = deescapepseudo ([escapedc]:s) cs
	deescapepseudo s (c1:c2:c3:cs)
		| [c1,c2,c3] == pseudoSlash = deescapepseudo ("/":s) cs
		| [c1,c2,c3] == pseudoBackslash = deescapepseudo ("\\":s) cs
		| [c1,c2,c3] == pseudoColon = deescapepseudo (":":s) cs
		| otherwise = deescapepseudo ([c1]:s) (c2:c3:cs)
	deescapepseudo s cs = concat (reverse (cs:s))

prop_viewPath_roundtrips :: MetaValue -> Bool
prop_viewPath_roundtrips v = fromViewPath (toViewPath v) == v

pathProduct :: [[FilePath]] -> [FilePath]
pathProduct [] = []
pathProduct (l:ls) = foldl combinel l ls
  where
	combinel xs ys = [combine x y | x <- xs, y <- ys]

{- Extracts the metadata from a ViewedFile, based on the view that was used
 - to construct it.
 -
 - Derived metadata is excluded.
 -}
fromView :: View -> ViewedFile -> MetaData
fromView view f = MetaData $ m `M.difference` derived
  where
	m = M.fromList $ map convfield $
		filter (not . isviewunset) (zip visible values)
	visible = filter viewVisible (viewComponents view)
	paths = splitDirectories (dropFileName f)
	values = map (S.singleton . fromViewPath) paths
	MetaData derived = getViewedFileMetaData f
	convfield (vc, v) = (viewField vc, v)

	-- When a directory is the one used to hold files that don't
	-- have the metadata set, don't include it in the MetaData.
	isviewunset (vc, v) = case viewFilter vc of
		FilterValues {} -> False
		FilterGlob {} -> False
		ExcludeValues {} -> False
		FilterValuesOrUnset _ (ViewUnset vu) -> isviewunset' vu v
		FilterGlobOrUnset _ (ViewUnset vu) -> isviewunset' vu v
	isviewunset' vu v = S.member (fromViewPath vu) v

{- Constructing a view that will match arbitrary metadata, and applying
 - it to a file yields a set of ViewedFile which all contain the same
 - MetaFields that were present in the input metadata
 - (excluding fields that are not visible). -}
prop_view_roundtrips :: AssociatedFile -> MetaData -> Bool -> Bool
prop_view_roundtrips (AssociatedFile Nothing) _ _ = True
prop_view_roundtrips (AssociatedFile (Just f)) metadata visible = or
	[ B.null (P.takeFileName f) && B.null (P.takeDirectory f)
	, viewTooLarge view
	, all hasfields (viewedFiles view viewedFileFromReference (fromRawFilePath f) metadata)
	]
  where
	view = View (Git.Ref "foo") $
		map (\(mf, mv) -> ViewComponent mf (FilterValues $ S.filter (not . B.null . fromMetaValue) mv) visible)
			(fromMetaData metadata)
	visiblefields = sort (map viewField $ filter viewVisible (viewComponents view))
	hasfields fv = sort (map fst (fromMetaData (fromView view fv))) == visiblefields

{- A directory foo/bar/baz/ is turned into metadata fields
 - /=foo, foo/=bar, foo/bar/=baz.
 -
 - Note that this may generate MetaFields that legalField rejects.
 - This is necessary to have a 1:1 mapping between directory names and
 - fields. So this MetaData cannot safely be serialized. -}
getDirMetaData :: FilePath -> MetaData
getDirMetaData d = MetaData $ M.fromList $ zip fields values
  where
	dirs = splitDirectories d
	fields = map (mkMetaFieldUnchecked . T.pack . addTrailingPathSeparator . joinPath)
		(inits dirs)
	values = map (S.singleton . toMetaValue . encodeBS . fromMaybe "" . headMaybe)
		(tails dirs)

getWorkTreeMetaData :: FilePath -> MetaData
getWorkTreeMetaData = getDirMetaData . dropFileName

getViewedFileMetaData :: FilePath -> MetaData
getViewedFileMetaData = getDirMetaData . dirFromViewedFile . takeFileName

{- Applies a view to the currently checked out branch, generating a new
 - branch for the view.
 -}
applyView :: View -> Maybe Adjustment -> Annex Git.Branch
applyView = applyView' viewedFileFromReference getWorkTreeMetaData

{- Generates a new branch for a View, which must be a more narrow
 - version of the View originally used to generate the currently
 - checked out branch. That is, it must match a subset of the files
 - in view, not any others.
 -}
narrowView :: View -> Maybe Adjustment -> Annex Git.Branch
narrowView = applyView' viewedFileReuse getViewedFileMetaData

{- Go through each staged file.
 - If the file is not annexed, skip it, unless it's a dotfile in the top,
 - or a file in a dotdir in the top. 
 - Look up the metadata of annexed files, and generate any ViewedFiles,
 - and stage them.
 -}
applyView' :: MkViewedFile -> (FilePath -> MetaData) -> View -> Maybe Adjustment -> Annex Git.Branch
applyView' mkviewedfile getfilemetadata view madj = do
	top <- fromRepo Git.repoPath
	(l, clean) <- inRepo $ Git.LsFiles.inRepoDetails [] [top]
	applyView'' mkviewedfile getfilemetadata view madj l clean $ 
		\(f, sha, mode) -> do
			topf <- inRepo (toTopFilePath f)
			k <- lookupKey f
			return (topf, sha, toTreeItemType mode, k)
	genViewBranch view madj

applyView''
	:: MkViewedFile
	-> (FilePath -> MetaData)
	-> View
	-> Maybe Adjustment
	-> [t]
	-> IO Bool
	-> (t -> Annex (TopFilePath, Sha, Maybe TreeItemType, Maybe Key))
	-> Annex ()
applyView'' mkviewedfile getfilemetadata view madj l clean conv = do
	viewg <- withNewViewIndex gitRepo
	withUpdateIndex viewg $ \uh -> do
		g <- Annex.gitRepo
		gc <- Annex.getGitConfig
		-- Streaming the metadata like this is an optimisation.
		catObjectStream g $ \mdfeeder mdcloser mdreader -> do
			tid <- liftIO . async =<< forkState
				(getmetadata gc mdfeeder mdcloser l)
			process uh mdreader
			join (liftIO (wait tid))
			liftIO $ void clean
  where
	genviewedfiles = viewedFiles view mkviewedfile -- enables memoization

	getmetadata _ _ mdcloser [] = liftIO mdcloser
	getmetadata gc mdfeeder mdcloser (t:ts) = do
		v@(topf, _sha, _treeitemtype, mkey) <- conv t
		let feed mdlogf = liftIO $ mdfeeder
			(v, Git.Ref.branchFileRef Annex.Branch.fullname mdlogf)
		case mkey of
			Just key -> feed (metaDataLogFile gc key)
			Nothing
				-- Handle toplevel dotfiles that are not
				-- annexed files by feeding through a query
				-- for dummy metadata. Calling
				-- Git.UpdateIndex.streamUpdateIndex'
				-- here would race with process's calls
				-- to it.
				| "." `B.isPrefixOf` getTopFilePath topf ->
					feed "dummy"
				| otherwise -> noop
		getmetadata gc mdfeeder mdcloser ts

	process uh mdreader = liftIO mdreader >>= \case
		Just ((topf, _, mtreeitemtype, Just k), mdlog) -> do
			let metadata = maybe emptyMetaData parseCurrentMetaData mdlog
			let f = fromRawFilePath $ getTopFilePath topf
			let metadata' = getfilemetadata f `unionMetaData` metadata
			forM_ (genviewedfiles f metadata') $ \fv -> do
				f' <- fromRepo (fromTopFilePath $ asTopFilePath $ toRawFilePath fv)
				stagefile uh f' k mtreeitemtype
			process uh mdreader
		Just ((topf, sha, Just treeitemtype, Nothing), _) -> do
			liftIO $ Git.UpdateIndex.streamUpdateIndex' uh $
				pureStreamer $ updateIndexLine sha treeitemtype topf
			process uh mdreader
		Just _ -> process uh mdreader
		Nothing -> return ()
	
	stagefile uh f k mtreeitemtype = case madj of
		Nothing -> stagesymlink uh f k
		Just (LinkAdjustment UnlockAdjustment) ->
			stagepointerfile uh f k mtreeitemtype
		Just (LinkPresentAdjustment UnlockPresentAdjustment) ->
			ifM (inAnnex k)
				( stagepointerfile uh f k mtreeitemtype
				, stagesymlink uh f k
				)
		Just (PresenceAdjustment HideMissingAdjustment (Just UnlockAdjustment)) ->
			whenM (inAnnex k) $
				stagepointerfile uh f k mtreeitemtype
		Just (PresenceAdjustment HideMissingAdjustment _) ->
			whenM (inAnnex k) $
				stagesymlink uh f k
		_ -> stagesymlink uh f k

	stagesymlink uh f k = do
		linktarget <- calcRepo (gitAnnexLink f k)
		sha <- hashSymlink linktarget
		liftIO . Git.UpdateIndex.streamUpdateIndex' uh
			=<< inRepo (Git.UpdateIndex.stageSymlink f sha)
	
	stagepointerfile uh f k mtreeitemtype = do
		let treeitemtype = if mtreeitemtype == Just TreeExecutable
			then TreeExecutable
			else TreeFile
		sha <- hashPointerFile k
		liftIO . Git.UpdateIndex.streamUpdateIndex' uh
			=<< inRepo (Git.UpdateIndex.stageFile sha treeitemtype f)

{- Updates the current view with any changes that have been made to its
 - parent branch or the metadata since the view was created or last updated.
 -
 - When there were changes, returns a ref to a commit for the updated view.
 - Does not update the view branch with it.
 -
 - This is not very optimised. An incremental update would be possible to
 - implement and would be faster, but more complicated.
 -}
updateView :: View -> Maybe Adjustment -> Annex (Maybe Git.Ref)
updateView view madj = do
	(l, clean) <- inRepo $ Git.LsTree.lsTree
		Git.LsTree.LsTreeRecursive
		(Git.LsTree.LsTreeLong True)
		(viewParentBranch view)
	applyView'' viewedFileFromReference getWorkTreeMetaData view madj l clean $
		\ti -> do
			let ref = Git.Ref.branchFileRef (viewParentBranch view)
				(getTopFilePath (Git.LsTree.file ti))
			k <- case Git.LsTree.size ti of
				Nothing -> catKey ref
				Just sz -> catKey' ref sz
			return
				( (Git.LsTree.file ti)
				, (Git.LsTree.sha ti)
				, (toTreeItemType (Git.LsTree.mode ti))
				, k
				)
	oldcommit <- inRepo $ Git.Ref.sha (branchView view madj)
	oldtree <- maybe (pure Nothing) (inRepo . Git.Ref.tree) oldcommit
	newtree <- withViewIndex $ inRepo Git.Branch.writeTree
	if oldtree /= Just newtree
		then Just <$> do
			cmode <- annexCommitMode <$> Annex.getGitConfig
			let msg = "updated " ++ fromRef (branchView view madj)
			let parent = catMaybes [oldcommit]
			inRepo (Git.Branch.commitTree cmode msg parent newtree)
		else return Nothing

{- Diff between currently checked out branch and staged changes, and
 - update metadata to reflect the changes that are being committed to the
 - view.
 -
 - Adding a file to a directory adds the metadata represented by
 - that directory to the file, and removing a file from a directory
 - removes the metadata.
 -
 - Note that removes must be handled before adds. This is so
 - that moving a file from x/foo/ to x/bar/ adds back the metadata for x.
 -}
withViewChanges :: (ViewedFile -> Key -> CommandStart) -> (ViewedFile -> Key -> CommandStart) -> Annex ()
withViewChanges addmeta removemeta = do
	(diffs, cleanup) <- inRepo $ DiffTree.diffIndex Git.Ref.headRef
	forM_ diffs handleremovals
	forM_ diffs handleadds
	void $ liftIO cleanup
  where
	handleremovals item
		| DiffTree.srcsha item `notElem` nullShas =
			handlechange item removemeta
				=<< catKey (DiffTree.srcsha item)
		| otherwise = noop
	handleadds item
		| DiffTree.dstsha item `notElem` nullShas = 
			handlechange item addmeta
				=<< catKey (DiffTree.dstsha item)
		| otherwise = noop
	handlechange item a = maybe noop
		(void . commandAction . a (fromRawFilePath $ getTopFilePath $ DiffTree.file item))

{- Runs an action using the view index file.
 - Note that the file does not necessarily exist, or can contain
 - info staged for an old view. -}
withViewIndex :: Annex a -> Annex a
withViewIndex = withIndexFile ViewIndexFile . const

withNewViewIndex :: Annex a -> Annex a
withNewViewIndex a = do
	liftIO . removeWhenExistsWith R.removeLink =<< fromRepo gitAnnexViewIndex
	withViewIndex a

{- Generates a branch for a view, using the view index file
 - to make a commit to the view branch. The view branch is not
 - checked out, but entering it will display the view. -}
genViewBranch :: View -> Maybe Adjustment -> Annex Git.Branch
genViewBranch view madj = withViewIndex $ do
	let branch = branchView view madj
	cmode <- annexCommitMode <$> Annex.getGitConfig
	void $ inRepo $ Git.Branch.commit cmode True (fromRef branch) branch []
	return branch

withCurrentView :: (View -> Maybe Adjustment -> Annex a) -> Annex a
withCurrentView a = maybe (giveup "Not in a view.") (uncurry a) =<< currentView
