{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.MetaData where

import Common.Annex
import Command
import Logs.MetaData
import Types.MetaData

import qualified Data.Set as S

def :: [Command]
def = [command "metadata" (paramPair paramFile (paramRepeating "FIELD[+-]=VALUE")) seek
	SectionUtility "sets metadata of a file"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (file:settings) = ifAnnexed file
	go
	(error $ "not an annexed file, so cannot add metadata: " ++ file)
  where
	go (k, _b) = do
		showStart "metadata" file
		next $ perform k (map parse settings)
start _ = error "specify a file and the metadata to set"

perform :: Key -> [Action] -> CommandPerform
perform k [] = next $ cleanup k
perform k as = do
	oldm <- getCurrentMetaData k
	let m = foldr (apply oldm) newMetaData as
	addMetaData k m
	next $ cleanup k
	
cleanup :: Key -> CommandCleanup
cleanup k = do
	m <- getCurrentMetaData k
	showLongNote $ unlines $ concatMap showmeta $ fromMetaData $ currentMetaData m
	return True
  where
	showmeta (f, vs) = map (\v -> fromMetaField f ++ "=" ++ fromMetaValue v) $ S.toList vs

data Action
	= AddMeta MetaField MetaValue
	| DelMeta MetaField MetaValue
	| SetMeta MetaField MetaValue

parse :: String -> Action
parse p = case lastMaybe f of
	Just '+' -> AddMeta (mkf f') v
	Just '-' -> DelMeta (mkf f') v
	_ -> SetMeta (mkf f) v
  where
	(f, sv) = separate (== '=') p
	f' = beginning f
	v = toMetaValue sv
	mkf fld = fromMaybe (badfield fld) (toMetaField fld)
	badfield fld = error $ "Illegal metadata field name, \"" ++ fld ++ "\""

apply :: MetaData -> Action -> MetaData -> MetaData
apply _ (AddMeta f v) m = updateMetaData f v m
apply _ (DelMeta f oldv) m = updateMetaData f (unsetMetaValue oldv) m
apply oldm (SetMeta f v) m = updateMetaData f v $
	foldr (updateMetaData f) m $
		map unsetMetaValue $ S.toList $ currentMetaDataValues f oldm
