{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Expire where

import Command
import Logs.Activity
import Logs.UUID
import Logs.MapLog
import Logs.Trust
import Annex.UUID
import qualified Remote
import Utility.HumanTime

import Data.Time.Clock.POSIX
import qualified Data.Map as M

cmd :: Command
cmd = command "expire" SectionMaintenance
	"expire inactive repositories"
	paramExpire (seek <$$> optParser)

paramExpire :: String
paramExpire = (paramRepeating $ paramOptional paramRemote ++ ":" ++ paramTime)

data ExpireOptions = ExpireOptions
	{ expireParams :: CmdParams
	, activityOption :: Maybe Activity
	, noActOption :: Bool
	}

optParser :: CmdParamsDesc -> Parser ExpireOptions
optParser desc = ExpireOptions
	<$> cmdParams desc
	<*> optional (option (str >>= parseActivity)
		( long "activity" <> metavar paramName
		<> help "specify activity that prevents expiry"
		))
	<*> switch
		( long "no-act"
		<> help "don't really do anything"
		)

seek :: ExpireOptions -> CommandSeek
seek o = do
	expire <- parseExpire (expireParams o)
	actlog <- lastActivities (activityOption o)
	u <- getUUID
	us <- filter (/= u) . M.keys <$> uuidMap
	descs <- uuidMap
	seekActions $ pure $ map (start expire (noActOption o) actlog descs) us

start :: Expire -> Bool -> Log Activity -> M.Map UUID String -> UUID -> CommandStart
start (Expire expire) noact actlog descs u =
	case lastact of
		Just ent | notexpired ent -> checktrust (== DeadTrusted) $ do
			showStart "unexpire" desc
			showNote =<< whenactive
			unless noact $
				trustSet u SemiTrusted
		_ -> checktrust (/= DeadTrusted) $ do
			showStart "expire" desc
			showNote =<< whenactive
			unless noact $
				trustSet u DeadTrusted
  where
	lastact = changed <$> M.lookup u actlog
	whenactive = case lastact of
		Just (Date t) -> do
			d <- liftIO $ durationSince $ posixSecondsToUTCTime t
			return $ "last active: " ++ fromDuration d ++ " ago"
		_  -> return "no activity"
	desc = fromUUID u ++ " " ++ fromMaybe "" (M.lookup u descs)
	notexpired ent = case ent of
		Unknown -> False
		Date t -> case lookupexpire of
			Just (Just expiretime) -> t >= expiretime
			_ -> True
	lookupexpire = headMaybe $ catMaybes $
		map (`M.lookup` expire) [Just u, Nothing]
	checktrust want a = ifM (want <$> lookupTrust u)
		( do
			void a
			next $ next $ return True
		, stop
		)

data Expire = Expire (M.Map (Maybe UUID) (Maybe POSIXTime))

parseExpire :: [String] -> Annex Expire
parseExpire [] = error "Specify an expire time."
parseExpire ps = do
	now <- liftIO getPOSIXTime
	Expire . M.fromList <$> mapM (parse now) ps
  where
	parse now s = case separate (== ':') s of
		(t, []) -> return (Nothing, parsetime now t)
		(n, t) -> do
			r <- Remote.nameToUUID n
			return (Just r, parsetime now t)
	parsetime _ "never" = Nothing
	parsetime now s = case parseDuration s of
		Nothing -> error $ "bad expire time: " ++ s
		Just d -> Just (now - durationToPOSIXTime d)

parseActivity :: Monad m => String -> m Activity
parseActivity s = case readish s of
	Nothing -> fail $ "Unknown activity. Choose from: " ++ 
		unwords (map show [minBound..maxBound :: Activity])
	Just v -> return v

