{- git-annex command
 -
 - Copyright 2012-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Command.Vicfg where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment (getEnv)
import Data.Tuple (swap)
import Data.Char (isSpace)
import Data.Default
import Data.Ord

import Command
import Annex.Perms
import Types.TrustLevel
import Types.Group
import Logs.Trust
import Logs.Group
import Logs.PreferredContent
import Logs.Schedule
import Logs.Config
import Logs.NumCopies
import Logs.MaxSize
import Types.StandardGroups
import Types.ScheduledActivity
import Types.NumCopies
import Remote
import Git.Types (fromConfigKey, fromConfigValue)
import Utility.DataUnits
import qualified Utility.FileIO as F

cmd :: Command
cmd = command "vicfg" SectionSetup "edit configuration in git-annex branch"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = do
	f <- fromRepo gitAnnexTmpCfgFile
	createAnnexDirectory $ parentDir f
	cfg <- getCfg
	descs <- uuidDescriptions
	liftIO $ writeFile (fromOsPath f) $ genCfg cfg descs
	vicfg cfg f
	stop

vicfg :: Cfg -> OsPath -> Annex ()
vicfg curcfg f = do
	vi <- liftIO $ catchDefaultIO "vi" $ getEnv "EDITOR"
	unlessM (liftIO $ boolSystem "sh" (shparams vi)) $
		giveup $ vi ++ " exited nonzero; aborting"
	r <- liftIO $ parseCfg (defCfg curcfg) 
		. map decodeBS
		. fileLines'
		<$> F.readFile' f
	liftIO $ removeWhenExistsWith removeFile f
	case r of
		Left s -> do
			liftIO $ writeFile (fromOsPath f) s
			vicfg curcfg f
		Right newcfg -> setCfg curcfg newcfg
  where
	-- Allow EDITOR to be processed by the shell,
	-- so it can contain options.
	shparams editor = 
		[ Param "-c"
		, Param $ unwords [editor, shellEscape (fromOsPath f)]
		]

data Cfg = Cfg
	{ cfgTrustMap :: M.Map UUID (Down TrustLevel)
	, cfgGroupMap :: M.Map UUID (S.Set Group)
	, cfgPreferredContentMap :: M.Map UUID PreferredContentExpression
	, cfgRequiredContentMap :: M.Map UUID PreferredContentExpression
	, cfgGroupPreferredContentMap :: M.Map Group PreferredContentExpression
	, cfgScheduleMap :: M.Map UUID [ScheduledActivity]
	, cfgGlobalConfigs :: M.Map ConfigKey ConfigValue
	, cfgNumCopies :: Maybe NumCopies
	, cfgMinCopies :: Maybe MinCopies
	, cfgMaxSizeMap :: M.Map UUID (Maybe MaxSize)
	}

getCfg :: Annex Cfg
getCfg = Cfg
	<$> (M.map Down <$> trustMapRaw) -- without local trust overrides
	<*> (groupsByUUID <$> groupMap)
	<*> preferredContentMapRaw
	<*> requiredContentMapRaw
	<*> groupPreferredContentMapRaw
	<*> scheduleMap
	<*> loadGlobalConfig
	<*> getGlobalNumCopies
	<*> getGlobalMinCopies
	<*> (M.map Just <$> getMaxSizes)

setCfg :: Cfg -> Cfg -> Annex ()
setCfg curcfg newcfg = do
	let diff = diffCfg curcfg newcfg
	mapM_ (uncurry trustSet) $ M.toList $ M.map (\(Down v) -> v) $ cfgTrustMap diff
	mapM_ (uncurry groupSet) $ M.toList $ cfgGroupMap diff
	mapM_ (uncurry preferredContentSet) $ M.toList $ cfgPreferredContentMap diff
	mapM_ (uncurry requiredContentSet) $ M.toList $ cfgRequiredContentMap diff
	mapM_ (uncurry groupPreferredContentSet) $ M.toList $ cfgGroupPreferredContentMap diff
	mapM_ (uncurry scheduleSet) $ M.toList $ cfgScheduleMap diff
	mapM_ (uncurry setGlobalConfig) $ M.toList $ cfgGlobalConfigs diff
	maybe noop setGlobalNumCopies $ cfgNumCopies diff
	maybe noop setGlobalMinCopies $ cfgMinCopies diff
	mapM_ (uncurry setmaxsize) $ M.toList $ cfgMaxSizeMap diff
  where
	setmaxsize _u Nothing = noop
	setmaxsize u (Just sz) = recordMaxSize u sz

{- Default config has all the keys from the input config, but with their
 - default values. -}
defCfg :: Cfg -> Cfg
defCfg curcfg = Cfg
	{ cfgTrustMap = mapdef $ cfgTrustMap curcfg
	, cfgGroupMap = mapdef $ cfgGroupMap curcfg
	, cfgPreferredContentMap = mapdef $ cfgPreferredContentMap curcfg
	, cfgRequiredContentMap = mapdef $ cfgRequiredContentMap curcfg
	, cfgGroupPreferredContentMap = mapdef $ cfgGroupPreferredContentMap curcfg
	, cfgScheduleMap = mapdef $ cfgScheduleMap curcfg
	, cfgGlobalConfigs = mapdef $ cfgGlobalConfigs curcfg
	, cfgNumCopies = Nothing
	, cfgMinCopies = Nothing
	, cfgMaxSizeMap = mapdef $ cfgMaxSizeMap curcfg
	}
  where
	mapdef :: forall k v. Default v => M.Map k v -> M.Map k v
	mapdef = M.map (const def)

diffCfg :: Cfg -> Cfg -> Cfg
diffCfg curcfg newcfg = Cfg
	{ cfgTrustMap = diff cfgTrustMap
	, cfgGroupMap = diff cfgGroupMap
	, cfgPreferredContentMap = diff cfgPreferredContentMap
	, cfgRequiredContentMap = diff cfgRequiredContentMap
	, cfgGroupPreferredContentMap = diff cfgGroupPreferredContentMap
	, cfgScheduleMap = diff cfgScheduleMap
	, cfgGlobalConfigs = diff cfgGlobalConfigs
	, cfgNumCopies = cfgNumCopies newcfg
	, cfgMinCopies = cfgMinCopies newcfg
	, cfgMaxSizeMap = diff cfgMaxSizeMap
	}
  where
	diff f = M.differenceWith (\x y -> if x == y then Nothing else Just x)
		(f newcfg) (f curcfg)

genCfg :: Cfg -> UUIDDescMap -> String
genCfg cfg descs = unlines $ intercalate [""]
	[ intro
	, trust
	, groups
	, preferredcontent
	, grouppreferredcontent
	, standardgroups
	, requiredcontent
	, maxsizes
	, schedule
	, numcopies
	, globalconfigs
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
		(\(Down t, u) -> line "trust" u $ showTrustLevel t)
		(\u -> lcom $ line "trust" u $ showTrustLevel def)
	  where
		trustlevels = unwords $ reverse $
			map showTrustLevel [minBound..maxBound]

	groups = settings cfg descs cfgGroupMap
		[ com "Repository groups"
		, com $ "(Standard groups: " ++ grouplist ++ ")"
		, com "(Separate group names with spaces)"
		]
		(\(s, u) -> line "group" u $ unwords $ map fromGroup $ S.toList s)
		(\u -> lcom $ line "group" u "")
	  where
		grouplist = unwords $ map (fromGroup . fromStandardGroup) [minBound..]

	preferredcontent = settings cfg descs cfgPreferredContentMap
		[ com "Repository preferred contents"
		, com "(Set to \"standard\" to use a repository's group's preferred contents)"
		]
		(\(s, u) -> line "wanted" u s)
		(\u -> line "wanted" u "")
	
	requiredcontent = settings cfg descs cfgRequiredContentMap
		[ com "Repository required contents" ]
		(\(s, u) -> line "required" u s)
		(\u -> line "required" u "")

	grouppreferredcontent = settings' cfg allgroups cfgGroupPreferredContentMap
		[ com "Group preferred contents"
		, com "(Used by repositories with \"groupwanted\" in their preferred contents)"
		]
		(\(s, g) -> gline g s)
		(\g -> gline g "")
	  where
		gline g val = [ unwords ["groupwanted", fromGroup g, "=", val] ]
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
			, fromGroup (fromStandardGroup g), "=", standardPreferredContent g
			]
	
	maxsizes = settings cfg descs cfgMaxSizeMap
		[ com "Maximum repository sizes"
		]
		(\(sz, u) -> line "maxsize" u $ maybe "" (\(MaxSize n) -> preciseSize storageUnits False n) sz)
		(\u -> line "maxsize" u "")
	
	schedule = settings cfg descs cfgScheduleMap
		[ com "Scheduled activities"
		, com "(Separate multiple activities with \"; \")"
		]
		(\(l, u) -> line "schedule" u $ fromScheduledActivities l)
		(\u -> line "schedule" u "")

	globalconfigs = settings' cfg S.empty cfgGlobalConfigs
		[ com "Other global configuration"
		]
		(\(s, g) -> gline g s)
		(\g -> gline g mempty)
	  where
		gline k v = [ unwords ["config", fromConfigKey k, "=", fromConfigValue v] ]

	line setting u val =
		[ com $ "(for " ++ fromUUIDDesc (fromMaybe mempty (M.lookup u descs)) ++ ")"
		, unwords [setting, fromUUID u, "=", val]
		]

	line' setting Nothing = com $ unwords [setting, "default", "="]
	line' setting (Just val) = unwords [setting, "default", "=", val]

	numcopies =
		[ com "Numcopies configuration"
		, line' "numcopies" (show . fromNumCopies <$> cfgNumCopies cfg)
		, line' "mincopies" (show . fromMinCopies <$> cfgMinCopies cfg)
		]
	
settings :: Ord v => Cfg -> UUIDDescMap -> (Cfg -> M.Map UUID v) -> [String] -> ((v, UUID) -> [String]) -> (UUID -> [String]) -> [String]
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
parseCfg :: Cfg -> [String] -> Either String Cfg
parseCfg defcfg = go [] defcfg
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
		| otherwise = parsed cfg f setting val'
	  where
		(setting, rest) = separate isSpace l
		(r, val) = separate (== '=') rest
		val' = trimspace val
		f = reverse $ trimspace $ reverse $ trimspace r
		trimspace = dropWhile isSpace

	parsed cfg f setting val
		| setting == "trust" = case readTrustLevel val of
			Nothing -> badval "trust value" val
			Just t ->
				let m = M.insert u (Down t) (cfgTrustMap cfg)
				in Right $ cfg { cfgTrustMap = m }
		| setting == "group" =
			let m = M.insert u (S.fromList $ map toGroup $ words val) (cfgGroupMap cfg)
			in Right $ cfg { cfgGroupMap = m }
		| setting == "wanted" = 
			case checkPreferredContentExpression val of
				Just e -> Left e
				Nothing ->
					let m = M.insert u val (cfgPreferredContentMap cfg)
					in Right $ cfg { cfgPreferredContentMap = m }
		| setting == "required" = 
			case checkPreferredContentExpression val of
				Just e -> Left e
				Nothing ->
					let m = M.insert u val (cfgRequiredContentMap cfg)
					in Right $ cfg { cfgRequiredContentMap = m }
		| setting == "groupwanted" =
			case checkPreferredContentExpression val of
				Just e -> Left e
				Nothing ->
					let m = M.insert (toGroup f) val (cfgGroupPreferredContentMap cfg)
					in Right $ cfg { cfgGroupPreferredContentMap = m }
		| setting == "maxsize" = case readSize dataUnits val of
			Nothing -> Left "parse error (expected a size such as \"100 gb\")"
			Just n ->
				let m = M.insert u (Just (MaxSize n)) (cfgMaxSizeMap cfg)
				in Right $ cfg { cfgMaxSizeMap = m }
		| setting == "schedule" = case parseScheduledActivities val of
			Left e -> Left e
			Right l -> 
				let m = M.insert u l (cfgScheduleMap cfg)
				in Right $ cfg { cfgScheduleMap = m }
		| setting == "config" =
			let m = M.insert (ConfigKey (encodeBS f)) (ConfigValue (encodeBS val)) (cfgGlobalConfigs cfg)
			in Right $ cfg { cfgGlobalConfigs = m }
		| setting == "numcopies" = case readish val of
			Nothing -> Left "parse error (expected an integer)"
			Just n -> Right $ cfg { cfgNumCopies = Just (configuredNumCopies n) }
		| setting == "mincopies" = case readish val of
			Nothing -> Left "parse error (expected an integer)"
			Just n -> Right $ cfg { cfgMinCopies = Just (configuredMinCopies n) }
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
