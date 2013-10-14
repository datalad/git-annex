{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Vicfg where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getEnv)
import Data.Tuple (swap)
import Data.Char (isSpace)

import Common.Annex
import Command
import Annex.Perms
import Types.TrustLevel
import Types.Group
import Logs.Trust
import Logs.Group
import Logs.PreferredContent
import Logs.Schedule
import Types.StandardGroups
import Types.ScheduledActivity
import Remote

def :: [Command]
def = [command "vicfg" paramNothing seek
	SectionSetup "edit git-annex's configuration"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	f <- fromRepo gitAnnexTmpCfgFile
	createAnnexDirectory $ parentDir f
	cfg <- getCfg
	descs <- uuidDescriptions
	liftIO $ writeFile f $ genCfg cfg descs
	vicfg cfg f
	stop

vicfg :: Cfg -> FilePath -> Annex ()
vicfg curcfg f = do
	vi <- liftIO $ catchDefaultIO "vi" $ getEnv "EDITOR"
	-- Allow EDITOR to be processed by the shell, so it can contain options.
	unlessM (liftIO $ boolSystem "sh" [Param "-c", Param $ unwords [vi, shellEscape f]]) $
		error $ vi ++ " exited nonzero; aborting"
	r <- parseCfg curcfg <$> liftIO (readFileStrict f)
	liftIO $ nukeFile f
	case r of
		Left s -> do
			liftIO $ writeFile f s
			vicfg curcfg f
		Right newcfg -> setCfg curcfg newcfg

data Cfg = Cfg
	{ cfgTrustMap :: TrustMap
	, cfgGroupMap :: M.Map UUID (S.Set Group)
	, cfgPreferredContentMap :: M.Map UUID String
	, cfgScheduleMap :: M.Map UUID [ScheduledActivity]
	}

getCfg :: Annex Cfg
getCfg = Cfg
	<$> trustMapRaw -- without local trust overrides
	<*> (groupsByUUID <$> groupMap)
	<*> preferredContentMapRaw
	<*> scheduleMap

setCfg :: Cfg -> Cfg -> Annex ()
setCfg curcfg newcfg = do
	let (trustchanges, groupchanges, preferredcontentchanges, schedulechanges) = diffCfg curcfg newcfg
	mapM_ (uncurry trustSet) $ M.toList trustchanges
	mapM_ (uncurry groupSet) $ M.toList groupchanges
	mapM_ (uncurry preferredContentSet) $ M.toList preferredcontentchanges
	mapM_ (uncurry scheduleSet) $ M.toList schedulechanges

diffCfg :: Cfg -> Cfg -> (TrustMap, M.Map UUID (S.Set Group), M.Map UUID String, M.Map UUID [ScheduledActivity])
diffCfg curcfg newcfg = (diff cfgTrustMap, diff cfgGroupMap, diff cfgPreferredContentMap, diff cfgScheduleMap)
  where
	diff f = M.differenceWith (\x y -> if x == y then Nothing else Just x)
		(f newcfg) (f curcfg)

genCfg :: Cfg -> M.Map UUID String -> String
genCfg cfg descs = unlines $ concat
	[intro, trust, groups, preferredcontent, schedule]
  where
	intro =
		[ com "git-annex configuration"
		, com ""
		, com "Changes saved to this file will be recorded in the git-annex branch."
		, com ""
		, com "Lines in this file have the format:"
		, com "  setting uuid = value"
		]

	trust = settings cfgTrustMap
		[ ""
		, com "Repository trust configuration"
		, com "(Valid trust levels: " ++ trustlevels ++ ")"
		]
		(\(t, u) -> line "trust" u $ showTrustLevel t)
		(\u -> lcom $ line "trust" u $ showTrustLevel SemiTrusted)
	  where
	  	trustlevels = unwords $ map showTrustLevel [Trusted .. DeadTrusted]

	groups = settings cfgGroupMap
		[ ""
		, com "Repository groups"
		, com $ "(Standard groups: " ++ grouplist ++ ")"
		, com "(Separate group names with spaces)"
		]
		(\(s, u) -> line "group" u $ unwords $ S.toList s)
		(\u -> lcom $ line "group" u "")
	  where
	  	grouplist = unwords $ map fromStandardGroup [minBound..]

	preferredcontent = settings cfgPreferredContentMap
		[ ""
		, com "Repository preferred contents"
		]
		(\(s, u) -> line "content" u s)
		(\u -> line "content" u "")

	schedule = settings cfgScheduleMap
		[ ""
		, com "Scheduled activities"
		, com "(Separate multiple activities with \"; \")"
		]
		(\(l, u) -> line "schedule" u $ fromScheduledActivities l)
		(\u -> line "schedule" u "")

	settings field desc showvals showdefaults = concat
		[ desc
		, concatMap showvals $ sort $ map swap $ M.toList $ field cfg
		, concatMap (lcom . showdefaults) $ missing field
		]

	line setting u value =
		[ com $ "(for " ++ fromMaybe "" (M.lookup u descs) ++ ")"
		, unwords [setting, fromUUID u, "=", value]
		]
	lcom = map (\l -> if "#" `isPrefixOf` l then l else '#' : l)
	missing field = S.toList $ M.keysSet descs `S.difference` M.keysSet (field cfg)

{- If there's a parse error, returns a new version of the file,
 - with the problem lines noted. -}
parseCfg :: Cfg -> String -> Either String Cfg
parseCfg curcfg = go [] curcfg . lines
  where
	go c cfg []
		| null (mapMaybe fst c) = Right cfg
		| otherwise = Left $ unlines $
			badheader ++ concatMap showerr (reverse c)
	go c cfg (l:ls) = case parse (dropWhile isSpace l) cfg of
		Left msg -> go ((Just msg, l):c) cfg ls
		Right cfg' -> go ((Nothing, l):c) cfg' ls

	parse l cfg
		| null l = Right cfg
		| "#" `isPrefixOf` l = Right cfg
		| null setting || null u = Left "missing repository uuid"
		| otherwise = handle cfg (toUUID u) setting value'
	  where
		(setting, rest) = separate isSpace l
		(r, value) = separate (== '=') rest
		value' = trimspace value
		u = reverse $ trimspace $ reverse $ trimspace r
		trimspace = dropWhile isSpace

	handle cfg u setting value
		| setting == "trust" = case readTrustLevel value of
			Nothing -> badval "trust value" value
			Just t ->
				let m = M.insert u t (cfgTrustMap cfg)
				in Right $ cfg { cfgTrustMap = m }
		| setting == "group" =
			let m = M.insert u (S.fromList $ words value) (cfgGroupMap cfg)
			in Right $ cfg { cfgGroupMap = m }
		| setting == "content" = 
			case checkPreferredContentExpression value of
				Just e -> Left e
				Nothing ->
					let m = M.insert u value (cfgPreferredContentMap cfg)
					in Right $ cfg { cfgPreferredContentMap = m }
		| setting == "schedule" = case parseScheduledActivities value of
			Left e -> Left e
			Right l -> 
				let m = M.insert u l (cfgScheduleMap cfg)
				in Right $ cfg { cfgScheduleMap = m }
		| otherwise = badval "setting" setting

	showerr (Just msg, l) = [parseerr ++ msg, l]
	showerr (Nothing, l)
		-- filter out the header and parse error lines
		-- from any previous parse failure
		| any (`isPrefixOf` l) (parseerr:badheader) = []
		| otherwise = [l]

	badval desc val = Left $ "unknown " ++ desc ++ " \"" ++ val ++ "\""
	badheader = 
		[ com "There was a problem parsing your input."
		, com "Search for \"Parse error\" to find the bad lines."
		, com "Either fix the bad lines, or delete them (to discard your changes)."
		]
	parseerr = com "Parse error in next line: "

com :: String -> String
com s = "# " ++ s
