{- git-annex-shell main program
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell where

import Annex.Common
import qualified Annex
import qualified Git.Construct
import qualified Git.Config
import CmdLine
import CmdLine.AnnexSetter
import Command
import Annex.UUID
import CmdLine.GitAnnexShell.Checks
import CmdLine.GitAnnexShell.Fields
import Remote.GCrypt (getGCryptUUID)
import P2P.Protocol (ServerMode(..))
import Git.Types
import qualified Types.Remote as R
import Logs.Proxy
import Logs.Cluster
import Logs.UUID
import Remote

import qualified Command.ConfigList
import qualified Command.NotifyChanges
import qualified Command.GCryptSetup
import qualified Command.P2PStdIO
import qualified Command.InAnnex
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.DropKey

import qualified Data.Map as M
import qualified Data.Set as S

cmdsMap :: M.Map ServerMode [Command]
cmdsMap = M.fromList $ map mk
	[ (ServeReadOnly, readonlycmds)
	, (ServeAppendOnly, appendcmds)
	, (ServeReadWrite, allcmds)
	]
  where
	readonlycmds = map addAnnexOptions
		[ notProxyable Command.ConfigList.cmd
		, gitAnnexShellCheck Command.NotifyChanges.cmd
		-- p2pstdio checks the environment variables to
		-- determine the security policy to use, so is safe to
		-- include in the readonly list even though it is not
		-- always readonly
		, gitAnnexShellCheck Command.P2PStdIO.cmd
		, notProxyable (gitAnnexShellCheck Command.InAnnex.cmd)
		, notProxyable (gitAnnexShellCheck Command.SendKey.cmd)
		]
	appendcmds = readonlycmds ++ map addAnnexOptions
		[ notProxyable (gitAnnexShellCheck Command.RecvKey.cmd)
		]
	allcmds = appendcmds ++ map addAnnexOptions
		[ notProxyable (gitAnnexShellCheck Command.DropKey.cmd)
		, notProxyable Command.GCryptSetup.cmd
		]

	mk (s, l) = (s, map (adddirparam . noMessages) l)
	adddirparam c = c { cmdparamdesc = "DIRECTORY " ++ cmdparamdesc c }

cmdsFor :: ServerMode -> [Command]
cmdsFor = fromMaybe [] . flip M.lookup cmdsMap

cmdsList :: [Command]
cmdsList = nub $ concat $ M.elems cmdsMap

addAnnexOptions :: Command -> Command
addAnnexOptions c = c { cmdannexoptions = commonShellOptions ++ cmdannexoptions c }

commonShellOptions :: [AnnexOption]
commonShellOptions = 
	annexOption (setAnnexState . checkUUID) (strOption
		( long "uuid" <> metavar paramUUID
		<> help "local repository uuid"
		))
	: commonOptions
  where
	checkUUID expected = getUUID >>= check
	  where
		check NoUUID = checkGCryptUUID expected
		check u 
			| u == toUUID expected = noop
			| otherwise = 
				unlessM (checkProxy (toUUID expected) u) $
					unexpectedUUID expected u
	
	checkGCryptUUID expected = check =<< getGCryptUUID True =<< gitRepo
	  where
		check (Just u) | u == toUUID expected = noop
		check Nothing = unexpected expected "uninitialized repository"
		check (Just u) = unexpectedUUID expected u
	
	unexpectedUUID expected u = unexpected expected $ "UUID " ++ fromUUID u
	unexpected expected s = giveup $
		"expected repository UUID " ++ expected ++ " but found " ++ s
				

run :: [String] -> IO ()
run [] = failure
-- skip leading -c options, passed by eg, ssh
run ("-c":p) = run p
-- a command can be either a builtin or something to pass to git-shell
run c@(cmd:dir:params)
	| cmd `elem` builtins = builtin cmd dir params
	| otherwise = external c
run c@(cmd:_)
	-- Handle the case of being the user's login shell. It will be passed
	-- a single string containing all the real parameters.
	| "git-annex-shell " `isPrefixOf` cmd = run $ drop 1 $ shellUnEscape cmd
	| cmd `elem` builtins = failure
	| otherwise = external c

failure :: IO ()
failure = giveup $ "bad parameters\n\n" ++ usage h cmdsList
  where
	h = "git-annex-shell [-c] command [parameters ...] [option ...]"

builtins :: [String]
builtins = map cmdname cmdsList

builtin :: String -> String -> [String] -> IO ()
builtin cmd dir params = do
	unless (cmd `elem` map cmdname (cmdsFor ServeReadOnly))
		checkNotReadOnly
	unless (cmd `elem` map cmdname (cmdsFor ServeAppendOnly))
		checkNotAppendOnly
	checkDirectory $ Just dir
	let (params', fieldparams, opts) = partitionParams params
	    rsyncopts = ("RsyncOptions", unwords opts)
	    fields = rsyncopts : filter checkField (parseFields fieldparams)
	dispatch False False (cmd : params') cmdsList fields mkrepo
		"git-annex-shell"
		"Restricted login shell for git-annex only SSH access"
  where
	mkrepo = do
		r <- Git.Construct.repoAbsPath (toRawFilePath dir)
			>>= Git.Construct.fromAbsPath
		let r' = r { repoPathSpecifiedExplicitly = True }
		Git.Config.read r'
			`catchIO` \_ -> do
				hn <- fromMaybe "unknown" <$> getHostname
				giveup $ "failed to read git config of git repository in " ++ hn ++ " on " ++ dir ++ "; perhaps this repository is not set up correctly or has moved"

external :: [String] -> IO ()
external params = do
	{- Normal git-shell commands all have the directory as their last
	 - parameter. -}
	let lastparam = lastMaybe =<< shellUnEscape <$> lastMaybe params
	    (params', _, _) = partitionParams params
	checkDirectory lastparam
	checkNotLimited
	unlessM (boolSystem "git-shell" $ map Param $ "-c":params') $
		giveup "git-shell failed"

{- Split the input list into 3 groups separated with a double dash --.
 - Parameters between two -- markers are field settings, in the form:
 - field=value field=value
 -
 - Parameters after the last -- are the command itself and its arguments e.g.,
 - rsync --bandwidth=100.
 -}
partitionParams :: [String] -> ([String], [String], [String])
partitionParams ps = case segment (== "--") ps of
	params:fieldparams:rest -> ( params, fieldparams, intercalate ["--"] rest )
	[params] -> (params, [], [])
	_ -> ([], [], [])

parseFields :: [String] -> [(String, String)]
parseFields = map (separate (== '='))

{- Only allow known fields to be set, ignore others.
 - Make sure that field values make sense. -}
checkField :: (String, String) -> Bool
checkField (field, val)
	| field == fieldName remoteUUID = fieldCheck remoteUUID val
	| field == fieldName autoInit = fieldCheck autoInit val
	| otherwise = False

{- Check if this repository can proxy for a specified remote uuid,
 - and if so enable proxying for it. -}
checkProxy :: UUID -> UUID -> Annex Bool
checkProxy remoteuuid ouruuid = M.lookup ouruuid <$> getProxies >>= \case
	Nothing -> return False
	-- This repository has (or had) proxying enabled. So it's
	-- ok to display error messages that talk about proxies.
	Just proxies ->
		case filter (\p -> proxyRemoteUUID p == remoteuuid) (S.toList proxies) of
			[] -> notconfigured
			ps -> case mkClusterUUID remoteuuid of
				Just cu -> proxyforcluster cu
				Nothing -> proxyfor ps
  where
	-- This repository may have multiple remotes that access the same
	-- repository. Proxy for the lowest cost one that is configured to
	-- be used as a proxy.
	proxyfor ps = do
		rs <- concat . byCost <$> remoteList
		myclusters <- annexClusters <$> Annex.getGitConfig
		let sameuuid r = uuid r == remoteuuid
		let samename r p = name r == proxyRemoteName p
		case headMaybe (filter (\r -> sameuuid r && proxyisconfigured rs myclusters r && any (samename r) ps) rs) of
			Nothing -> notconfigured
			Just r -> do
				Annex.changeState $ \st ->
					st { Annex.proxyremote = Just (Right r) }
				return True
	
	-- Only proxy for a remote when the git configuration
	-- allows it. This is important to prevent changes to 
	-- the git-annex branch making git-annex-shell unexpectedly
	-- proxy for remotes.
	proxyisconfigured rs myclusters r
		| remoteAnnexProxy (R.gitconfig r) = True
		-- Proxy for remotes that are configured as cluster nodes.
		| any (`M.member` myclusters) (fromMaybe [] $ remoteAnnexClusterNode $ R.gitconfig r) = True
		-- Proxy for a remote when it is proxied by another remote
		-- which is itself configured as a cluster gateway.
		| otherwise = case remoteAnnexProxiedBy (R.gitconfig r) of
			Just proxyuuid -> not $ null $ 
				concatMap (remoteAnnexClusterGateway . R.gitconfig) $
					filter (\p -> R.uuid p == proxyuuid) rs
			Nothing -> False

	proxyforcluster cu = do
		clusters <- getClusters
		if M.member cu (clusterUUIDs clusters)
			then do
				Annex.changeState $ \st ->
					st { Annex.proxyremote = Just (Left cu) }
				return True
			else notconfigured

	notconfigured = M.lookup remoteuuid <$> uuidDescMap >>= \case
		Just desc -> giveup $ "not configured to proxy for repository " ++ fromUUIDDesc desc
		Nothing -> return False
