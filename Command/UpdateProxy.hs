{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.UpdateProxy where

import Command
import qualified Annex
import Logs.Proxy
import Annex.UUID
import qualified Remote as R
import qualified Types.Remote as R
import Utility.SafeOutput
import Types.Cluster

import qualified Data.Map as M
import qualified Data.Set as S

cmd :: Command
cmd = noMessages $ command "updateproxy" SectionSetup 
	"update records with proxy configuration"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = startingCustomOutput (ActionItemOther Nothing) $ do
	rs <- R.remoteList
	let remoteproxies = S.fromList $ map mkproxy $
		filter (isproxy . R.gitconfig) rs
	clusterproxies <-
		(S.fromList . map mkclusterproxy . M.toList . annexClusters)
		<$> Annex.getGitConfig
	let proxies = remoteproxies <> clusterproxies
	u <- getUUID
	oldproxies <- fromMaybe mempty . M.lookup u <$> getProxies
	if oldproxies == proxies
		then liftIO $ putStrLn "No proxy changes to record."
		else do
			describechanges oldproxies proxies
			recordProxies proxies
	next $ return True
  where
	describechanges oldproxies proxies =
		forM_ (S.toList $ S.union oldproxies proxies) $ \p ->
			case (S.member p oldproxies, S.member p proxies) of
				(False, True) -> liftIO $
					putStrLn $ safeOutput $
						"Started proxying for " ++ proxyRemoteName p
				(True, False) -> liftIO $
					putStrLn $ safeOutput $
						"Stopped proxying for " ++ proxyRemoteName p
				_ -> noop
	
	isproxy c = remoteAnnexProxy c || not (null (remoteAnnexClusterNode c))
	
	mkproxy r = Proxy (R.uuid r) (R.name r)

	mkclusterproxy (remotename, cu) = 
		Proxy (fromClusterUUID cu) remotename
