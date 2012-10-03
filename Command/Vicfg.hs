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

import Common.Annex
import Command
import Annex.Perms
import Types.TrustLevel
import Types.Group
import Logs.Trust
import Logs.Group
import Remote

def :: [Command]
def = [command "vicfg" paramNothing seek
	"edit git-annex's configuration"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = do
	f <- fromRepo gitAnnexTmpCfgFile
	createAnnexDirectory (parentDir f)
	liftIO . writeFile f =<< genCfg <$> getCfg
	vicfg f
	stop

vicfg :: FilePath -> Annex ()
vicfg f = do
	vi <- liftIO $ catchDefaultIO "vi" $ getEnv "EDITOR"
	-- Allow EDITOR to be processed by the shell, so it can contain options.
	unlessM (liftIO $ boolSystem "sh" [Param "-c", Param $ unwords [vi, f]]) $
		error $ vi ++ " exited nonzero; aborting"
	r <- parseCfg <$> liftIO (readFileStrict f)
	liftIO $ nukeFile f
	case r of
		Left s -> do
			liftIO $ writeFile f s
			vicfg f
		Right c -> setCfg c

data Cfg = Cfg
	{ cfgTrustMap :: TrustMap
	, cfgGroupMap :: M.Map UUID (S.Set Group)
	, cfgDescriptions :: M.Map UUID String
	}

getCfg :: Annex Cfg
getCfg = Cfg
	<$> trustMapRaw -- without local trust overrides
	<*> (groupsByUUID <$> groupMap)
	<*> uuidDescriptions

setCfg :: Cfg -> Annex ()
setCfg = error "TODO setCfg"

genCfg :: Cfg -> String
genCfg cfg = unlines $ concat
	[intro, trustintro, trust, defaulttrust, groupsintro, groups, defaultgroups]
	where
		intro =
			[ com "git-annex configuration"
			, com ""
			, com "Changes saved to this file will be recorded in the git-annex branch."
			, com ""
			, com "Lines in this file have the format:"
			, com "  setting repo = value"
			]
		trustintro =
			[ ""
			, com "Repository trust configuration"
			, com "(Valid trust levels: " ++
			  unwords (map showTrustLevel [Trusted .. DeadTrusted]) ++
			  ")"
			]
		trust = map (\(t, u) -> line "trust" u $ showTrustLevel t) $
			sort $ map swap $ M.toList $ cfgTrustMap cfg
		defaulttrust = map (\u -> pcom $ line "trust" u $ showTrustLevel SemiTrusted) $
			missing cfgTrustMap
		groupsintro = 
			[ ""
			, com "Repository groups"
			, com "(Separate group names with spaces)"
			]
		groups = map (\(s, u) -> line "group" u $ unwords $ S.toList s) $
			sort $ map swap $ M.toList $ cfgGroupMap cfg
		defaultgroups = map (\u -> pcom $ line "group" u "") $
			missing cfgGroupMap
		line setting u value = unwords
			[ setting
			, showu u
			, "=" 
			, value
			]
		com s = "# " ++ s
		pcom s = "#" ++ s
		showu u = fromMaybe (fromUUID u) $
			M.lookup u (cfgDescriptions cfg)
		missing field = S.toList $ M.keysSet (cfgDescriptions cfg) `S.difference` M.keysSet (field cfg)

{- If there's a parse error, returns a new version of the file,
 - with the problem lines noted. -}
parseCfg :: String -> Either String Cfg
parseCfg = undefined
