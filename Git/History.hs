{-# LANGUAGE BangPatterns #-}

{- git commit history interface
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.History where

import Common
import Git
import Git.Command
import Git.Sha

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as L8

data History t = History t (S.Set (History t))
	deriving (Show, Eq, Ord)

mapHistory :: (Ord a, Ord b) => (a -> b) -> History a -> History b
mapHistory f (History t s) = History (f t) (S.map (mapHistory f) s)

historyDepth :: History t -> Integer
historyDepth (History _ s)
	| S.null s = 1
	| otherwise = 1 + maximum (map historyDepth (S.toList s))

truncateHistoryToDepth :: Ord t => Integer -> History t -> History t
truncateHistoryToDepth n (History t ps) = History t (go 1 ps)
  where
	go depth s
		| depth >= n = S.empty
		| otherwise =
			let depth' = succ depth
			in flip S.map s $ \(History t' s') ->
				History t' (go depth' s')


data HistoryCommit = HistoryCommit
	{ historyCommit :: Sha
	, historyCommitTree :: Sha
	, historyCommitParents :: [Sha]
	} deriving (Show, Eq, Ord)

{- Gets a History starting with the provided commit, and down to the
 - requested depth. -}
getHistoryToDepth :: Integer -> Ref -> Repo -> IO (Maybe (History HistoryCommit))
getHistoryToDepth n commit r = withCreateProcess p go
  where
	p = (gitCreateProcess params r)
		{ std_out = CreatePipe }
	go _ (Just inh) _ pid = do
		!h <- fmap (truncateHistoryToDepth n) 
			. build Nothing 
			. map parsehistorycommit
			. map L.toStrict
			. L8.lines
			<$> L.hGetContents inh
		hClose inh
		void $ waitForProcess pid
		return h
	go _ _ _ _ = error "internal"

	build h [] = fmap (mapHistory fst) h
	build _ (Nothing:_) = Nothing
	build Nothing (Just v:rest) =
		build (Just (History v S.empty)) rest
	build (Just h) (Just v:rest) =
		let h' = traverseadd v h
		in build (Just h') $
			-- detect when all parents down to desired depth
			-- have been found, and avoid processing any more,
			-- this makes it much faster when there is a lot of
			-- history.
			if parentsfound h' then [] else rest

	traverseadd v@(hc, _ps) (History v'@(hc', ps') s)
		| historyCommit hc `elem` ps' =
			let ps'' = filter (/= historyCommit hc) ps'
			in History (hc', ps'') (S.insert (History v S.empty) s)
		| otherwise = History v' (S.map (traverseadd v) s)

	parentsfound = parentsfound' 1
	parentsfound' depth (History (_hc, ps) s)
		| not (null ps) = False
		| null ps && depth == n = True
		| depth >= n = True
		| otherwise = all (parentsfound' (succ depth)) (S.toList s)

	params =
		[ Param "log"
		, Param (fromRef commit)
		, Param "--full-history"
		, Param "--no-abbrev"
		, Param "--format=%T %H %P"
		]
	
	parsehistorycommit l = case map extractSha (B8.split ' ' l) of
		(Just t:Just c:ps) -> Just $ 
			( HistoryCommit
				{ historyCommit = c
				, historyCommitTree = t
				, historyCommitParents = catMaybes ps
				}
			, catMaybes ps
			)
		_ -> Nothing
