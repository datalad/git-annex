{- git-annex command
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
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

seek :: CommandSeek
seek = withNothing start

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
	, cfgPreferredContentMap :: M.Map UUID PreferredContentExpression
	, cfgRequiredContentMap :: M.Map UUID PreferredContentExpression
	, cfgGroupPreferredContentMap :: M.Map Group PreferredContentExpression
	, cfgScheduleMap :: M.Map UUID [ScheduledActivity]
	}

getCfg :: Annex Cfg
getCfg = Cfg
	<$> trustMapRaw -- without local trust overrides
	<*> (groupsByUUID <$> groupMap)
	<*> preferredContentMapRaw
	<*> requiredContentMapRaw
	<*> groupPreferredContentMapRaw
	<*> scheduleMap

setCfg :: Cfg -> Cfg -> Annex ()
setCfg curcfg newcfg = do
	let diff = diffCfg curcfg newcfg
	mapM_ (uncurry trustSet) $ M.toList $ cfgTrustMap diff
	mapM_ (uncurry groupSet) $ M.toList $ cfgGroupMap diff
	mapM_ (uncurry preferredContentSet) $ M.toList $ cfgPreferredContentMap diff
	mapM_ (uncurry requiredContentSet) $ M.toList $ cfgRequiredContentMap diff
	mapM_ (uncurry groupPreferredContentSet) $ M.toList $ cfgGroupPreferredContentMap diff
	mapM_ (uncurry scheduleSet) $ M.toList $ cfgScheduleMap diff

diffCfg :: Cfg -> Cfg -> Cfg
diffCfg curcfg newcfg = Cfg
	{ cfgTrustMap = diff cfgTrustMap
	, cfgGroupMap = diff cfgGroupMap
	, cfgPreferredContentMap = diff cfgPreferredContentMap
	, cfgRequiredContentMap = diff cfgRequiredContentMap
	, cfgGroupPreferredContentMap = diff cfgGroupPreferredContentMap
	, cfgScheduleMap = diff cfgScheduleMap
	}
  where
	diff f = M.differenceWith (\x y -> if x == y then Nothing else Just x)
		(f newcfg) (f curcfg)

genCfg :: Cfg -> M.Map UUID String -> String
genCfg cfg descs = unlines $ intercalate [""]
	[ intro
	, trust
	, groups
	, preferredcontent
	, grouppreferredcontent
	, standardgroups
	, requiredcontent
	, schedule
	]
  where
	intro =
		[ com "git-annex configuration"
		, com ""
		, com "Changes saved to this file will be recorded in the git-annex branch."
		, com ""
		, com "Lines in this file have the format:"
		, com "  setting field = value"
		]

	trust = settings cfg descs cfgTrustMap
		[ com "Repository trust configuration"
		, com "(Valid trust levels: " ++ trustlevels ++ ")"
		]
		(\(t, u) -> line "trust" u $ showTrustLevel t)
		(\u -> lcom $ line "trust" u $ showTrustLevel SemiTrusted)
	  where
		trustlevels = unwords $ map showTrustLevel [Trusted .. DeadTrusted]

	groups = settings cfg descs cfgGroupMap
		[ com "Repository groups"
		, com $ "(Standard groups: " ++ grouplist ++ ")"
		, com "(Separate group names with spaces)"
		]
		(\(s, u) -> line "group" u $ unwords $ S.toList s)
		(\u -> lcom $ line "group" u "")
	  where
	  	grouplist = unwords $ map fromStandardGroup [minBound..]

	preferredcontent = settings cfg descs cfgPreferredContentMap
		[ com "Repository preferred contents" ]
		(\(s, u) -> line "wanted" u s)
		(\u -> line "wanted" u "standard")
	
	requiredcontent = settings cfg descs cfgRequiredContentMap
		[ com "Repository required contents" ]
		(\(s, u) -> line "required" u s)
		(\u -> line "required" u "")

	grouppreferredcontent = settings' cfg allgroups cfgGroupPreferredContentMap
		[ com "Group preferred contents"
		, com "(Used by repositories with \"groupwanted\" in their preferred contents)"
		]
		(\(s, g) -> gline g s)
		(\g -> gline g "standard")
	  where
	  	gline g value = [ unwords ["groupwanted", g, "=", value] ]
		allgroups = S.unions $ stdgroups : M.elems (cfgGroupMap cfg)
		stdgroups = S.fromList $ map fromStandardGroup [minBound..maxBound]

	standardgroups =
		[ com "Standard preferred contents"
		, com "(Used by wanted or groupwanted expressions containing \"standard\")"
		, com "(For reference only; built-in and cannot be changed!)"
		]
		++ map gline [minBound..maxBound]
	  where
		gline g = com $ unwords
			[ "standard"
			, fromStandardGroup g, "=", standardPreferredContent g
			]
	
	schedule = settings cfg descs cfgScheduleMap
		[ com "Scheduled activities"
		, com "(Separate multiple activities with \"; \")"
		]
		(\(l, u) -> line "schedule" u $ fromScheduledActivities l)
		(\u -> line "schedule" u "")

	line setting u value =
		[ com $ "(for " ++ fromMaybe "" (M.lookup u descs) ++ ")"
		, unwords [setting, fromUUID u, "=", value]
		]
	
settings :: Ord v => Cfg -> M.Map UUID String -> (Cfg -> M.Map UUID v) -> [String] -> ((v, UUID) -> [String]) -> (UUID -> [String]) -> [String]
settings cfg descs = settings' cfg (M.keysSet descs)

settings' :: (Ord v, Ord f) => Cfg -> S.Set f -> (Cfg -> M.Map f v) -> [String] -> ((v, f) -> [String]) -> (f -> [String]) -> [String]
settings' cfg s field desc showvals showdefaults = concat
	[ desc
	, concatMap showvals $ sort $ map swap $ M.toList $ field cfg
	, concatMap (lcom . showdefaults) missing
	]
  where
	missing = S.toList $ s `S.difference` M.keysSet (field cfg)

lcom :: [String] -> [String]
lcom = map (\l -> if "#" `isPrefixOf` l then l else '#' : l)

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
		| null setting || null f = Left "missing field"
		| otherwise = handle cfg f setting value'
	  where
		(setting, rest) = separate isSpace l
		(r, value) = separate (== '=') rest
		value' = trimspace value
		f = reverse $ trimspace $ reverse $ trimspace r
		trimspace = dropWhile isSpace

	handle cfg f setting value
		| setting == "trust" = case readTrustLevel value of
			Nothing -> badval "trust value" value
			Just t ->
				let m = M.insert u t (cfgTrustMap cfg)
				in Right $ cfg { cfgTrustMap = m }
		| setting == "group" =
			let m = M.insert u (S.fromList $ words value) (cfgGroupMap cfg)
			in Right $ cfg { cfgGroupMap = m }
		| setting == "wanted" = 
			case checkPreferredContentExpression value of
				Just e -> Left e
				Nothing ->
					let m = M.insert u value (cfgPreferredContentMap cfg)
					in Right $ cfg { cfgPreferredContentMap = m }
		| setting == "required" = 
			case checkPreferredContentExpression value of
				Just e -> Left e
				Nothing ->
					let m = M.insert u value (cfgRequiredContentMap cfg)
					in Right $ cfg { cfgRequiredContentMap = m }
		| setting == "groupwanted" =
			case checkPreferredContentExpression value of
				Just e -> Left e
				Nothing ->
					let m = M.insert f value (cfgGroupPreferredContentMap cfg)
					in Right $ cfg { cfgGroupPreferredContentMap = m }
		| setting == "schedule" = case parseScheduledActivities value of
			Left e -> Left e
			Right l -> 
				let m = M.insert u l (cfgScheduleMap cfg)
				in Right $ cfg { cfgScheduleMap = m }
		| otherwise = badval "setting" setting
	  where
		u = toUUID f

	showerr (Just msg, l) = [parseerr ++ msg, l]
	showerr (Nothing, l)
		-- filter out the header and parse error lines
		-- from any previous parse failure
		| any (`isPrefixOf` l) (parseerr:badheader) = []
		| otherwise = [l]

	badval desc val = Left $ "unknown " ++ desc ++ " \"" ++ val ++ "\""
	badheader = 
		[ com "** There was a problem parsing your input!"
		, com "** Search for \"Parse error\" to find the bad lines."
		, com "** Either fix the bad lines, or delete them (to discard your changes)."
		]
	parseerr = com "** Parse error in next line: "

com :: String -> String
com s = "# " ++ s
