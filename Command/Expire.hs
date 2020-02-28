{- git-annex command
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Expire where

import Command
import Logs.Activity
import Logs.UUID
import Logs.MapLog
import Logs.Trust
import Annex.UUID
import Annex.VectorClock
import qualified Remote
import Utility.HumanTime

import Control.Monad.Fail as Fail (MonadFail(..))
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
	us <- filter (/= u) . M.keys <$> uuidDescMap
	descs <- uuidDescMap
	commandActions $ map (start expire (noActOption o) actlog descs) us

start :: Expire -> Bool -> Log Activity -> UUIDDescMap -> UUID -> CommandStart
start (Expire expire) noact actlog descs u =
	case lastact of
		Just ent | notexpired ent -> checktrust (== DeadTrusted) $
			starting "unexpire" (ActionItemOther (Just desc)) $ do
				showNote =<< whenactive
				unless noact $
					trustSet u SemiTrusted
				next $ return True
		_ -> checktrust (/= DeadTrusted) $
			starting "expire" (ActionItemOther (Just desc)) $ do
				showNote =<< whenactive
				unless noact $
					trustSet u DeadTrusted
				next $ return True
  where
	lastact = changed <$> M.lookup u actlog
	whenactive = case lastact of
		Just (VectorClock c) -> do
			d <- liftIO $ durationSince $ posixSecondsToUTCTime c
			return $ "last active: " ++ fromDuration d ++ " ago"
		_  -> return "no activity"
	desc = fromUUID u ++ " " ++ fromUUIDDesc (fromMaybe mempty (M.lookup u descs))
	notexpired ent = case ent of
		Unknown -> False
		VectorClock c -> case lookupexpire of
			Just (Just expiretime) -> c >= expiretime
			_ -> True
	lookupexpire = headMaybe $ catMaybes $
		map (`M.lookup` expire) [Just u, Nothing]
	checktrust want = stopUnless (want <$> lookupTrust u)

data Expire = Expire (M.Map (Maybe UUID) (Maybe POSIXTime))

parseExpire :: [String] -> Annex Expire
parseExpire [] = giveup "Specify an expire time."
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
		Nothing -> giveup $ "bad expire time: " ++ s
		Just d -> Just (now - durationToPOSIXTime d)

parseActivity :: MonadFail m => String -> m Activity
parseActivity s = case readish s of
	Nothing -> Fail.fail $ "Unknown activity. Choose from: " ++ 
		unwords (map show [minBound..maxBound :: Activity])
	Just v -> return v

