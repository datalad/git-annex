{- git-annex recent views log
 -
 - The most recently accessed view comes first.
 -
 - This file is stored locally in .git/annex/, not in the git-annex branch.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.View (
	currentView,
	setView,
	recentViews,
	branchView,
	prop_branchView_legal,
) where

import Common.Annex
import Types.View
import Types.MetaData
import qualified Git
import qualified Git.Branch
import qualified Git.Ref
import Utility.Tmp

import qualified Data.Set as S
import Data.Char

showLog :: View -> String
showLog (View branch components) = show branch ++ " " ++ show components

parseLog :: String -> Maybe View
parseLog s = 
	let (branch, components) = separate (== ' ') s
	in View
		<$> pure (Git.Ref branch)
		<*> readish components

setView :: View -> Annex ()
setView v = do
	l <- take 99 . filter (/= v) <$> recentViews
	f <- fromRepo gitAnnexViewLog
	liftIO $ viaTmp writeFile f $ unlines $ map showLog (v : l)

recentViews :: Annex [View]
recentViews = do
	f <- fromRepo gitAnnexViewLog
	liftIO $ mapMaybe parseLog . lines <$> catchDefaultIO [] (readFile f)

{- Gets the currently checked out view, if there is one. -}
currentView :: Annex (Maybe View)
currentView = do
	vs <- recentViews
	maybe Nothing (go vs) <$> inRepo Git.Branch.current
  where
  	go [] _ = Nothing
	go (v:vs) b
		| branchView v == b = Just v
		| otherwise = go vs b

{- Generates a git branch name for a View.
 - 
 - There is no guarantee that each view gets a unique branch name,
 - but the branch name is used to express the view as well as possible.
 -}
branchView :: View -> Git.Branch
branchView view
	| null name = Git.Ref "refs/heads/views"
	| otherwise = Git.Ref $ "refs/heads/views/" ++ name
  where
	name = intercalate ";" $ map branchcomp (viewComponents view)
	branchcomp c
		| multiValue (viewFilter c) = branchcomp' c
		| otherwise = "(" ++ branchcomp' c ++ ")"
	branchcomp' (ViewComponent metafield viewfilter)
		| metafield == tagMetaField = branchvals viewfilter
		| otherwise = concat
			[ forcelegal (fromMetaField metafield)
			, "="
			, branchvals viewfilter
			]
	branchvals (FilterValues set) = forcelegal $
		intercalate "," $ map fromMetaValue $ S.toList set
	branchvals (FilterGlob glob) = forcelegal glob
	forcelegal s
		| Git.Ref.legal True s = s
		| otherwise = map (\c -> if isAlphaNum c then c else '_') s

prop_branchView_legal :: View -> Bool
prop_branchView_legal = Git.Ref.legal False . show . branchView
