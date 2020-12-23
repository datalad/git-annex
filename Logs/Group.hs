{- git-annex group log
 -
 - Copyright 2012, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Group (
	groupLog,
	groupChange,
	groupSet,
	lookupGroups,
	groupMap,
	groupMapLoad,
	getStandardGroup,
	inUnwantedGroup
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Data.ByteString.Builder

import Annex.Common
import Logs
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased
import Types.Group
import Types.StandardGroups

{- Returns the groups of a given repo UUID. -}
lookupGroups :: UUID -> Annex (S.Set Group)
lookupGroups u = (fromMaybe S.empty . M.lookup u) . groupsByUUID <$> groupMap

{- Applies a set modifier to change the groups for a uuid in the groupLog. -}
groupChange :: UUID -> (S.Set Group -> S.Set Group) -> Annex ()
groupChange uuid@(UUID _) modifier = do
	curr <- lookupGroups uuid
	c <- currentVectorClock
	Annex.Branch.change groupLog $
		buildLogOld buildGroup . changeLog c uuid (modifier curr) . parseLogOld parseGroup
	
	-- The changed group invalidates the preferred content cache.
	Annex.changeState $ \s -> s
		{ Annex.groupmap = Nothing
		, Annex.preferredcontentmap = Nothing
		}
groupChange NoUUID _ = error "unknown UUID; cannot modify"

buildGroup :: S.Set Group -> Builder
buildGroup = go . S.toList
  where
	go [] = mempty
	go (g:gs) = bld g <> mconcat [ charUtf8 ' ' <> bld g' | g' <- gs ]
	bld (Group g) = byteString g

parseGroup :: A.Parser (S.Set Group)
parseGroup = S.fromList <$> go []
  where
	go l = (A.endOfInput *> pure l)
		<|> ((getgroup <* A8.char ' ') >>= go . (:l))
		<|> ((:l) <$> getgroup)
		-- allow extra writespace before or after a group name
		<|> (A8.char ' ' >>= const (go l))
	getgroup = Group <$> A8.takeWhile1 (/= ' ')

groupSet :: UUID -> S.Set Group -> Annex ()
groupSet u g = groupChange u (const g)

{- The map is cached for speed. -}
groupMap :: Annex GroupMap
groupMap = maybe groupMapLoad return =<< Annex.getState Annex.groupmap

{- Loads the map, updating the cache. -}
groupMapLoad :: Annex GroupMap
groupMapLoad = do
	m <- makeGroupMap . simpleMap . parseLogOld parseGroup
		<$> Annex.Branch.get groupLog
	Annex.changeState $ \s -> s { Annex.groupmap = Just m }
	return m

makeGroupMap :: M.Map UUID (S.Set Group) -> GroupMap
makeGroupMap byuuid = GroupMap byuuid bygroup
  where
	bygroup = M.fromListWith S.union $
		concatMap explode $ M.toList byuuid
	explode (u, s) = map (\g -> (g, S.singleton u)) (S.toList s)

{- If a repository is in exactly one standard group, returns it. -}
getStandardGroup :: S.Set Group -> Maybe StandardGroup
getStandardGroup s = case mapMaybe toStandardGroup $ S.toList s of
	[g] -> Just g
	_ -> Nothing

inUnwantedGroup :: UUID -> Annex Bool
inUnwantedGroup u = elem UnwantedGroup 
	. mapMaybe toStandardGroup . S.toList <$> lookupGroups u
