{- Git configuration
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config where

import Data.Maybe
import Control.Monad.State (liftIO)

import qualified Git
import qualified Annex
import Types
import Utility

type ConfigKey = String

{- Changes a git config setting in both internal state and .git/config -}
setConfig :: ConfigKey -> String -> Annex ()
setConfig k value = do
	g <- Annex.gitRepo
	liftIO $ Git.run g "config" [Param k, Param value]
	-- re-read git config and update the repo's state
	g' <- liftIO $ Git.configRead g
	Annex.changeState $ \s -> s { Annex.repo = g' }

{- Looks up a per-remote config setting in git config.
 - Failing that, tries looking for a global config option. -}
getConfig :: Git.Repo -> ConfigKey -> String -> Annex String
getConfig r key def = do
	g <- Annex.gitRepo
	let def' = Git.configGet g ("annex." ++ key) def
	return $ Git.configGet g (remoteConfig r key) def'

remoteConfig :: Git.Repo -> ConfigKey -> String
remoteConfig r key = "remote." ++ fromMaybe "" (Git.repoRemoteName r) ++ ".annex-" ++ key

{- Calculates cost for a remote.
 -
 - The default cost is 100 for local repositories, and 200 for remote
 - repositories; it can also be configured by remote.<name>.annex-cost
 -}
remoteCost :: Git.Repo -> Int -> Annex Int
remoteCost r def = do
	c <- getConfig r "cost" ""
	if not $ null c
		then return $ read c
		else return def

cheapRemoteCost :: Int
cheapRemoteCost = 100
semiCheapRemoteCost :: Int
semiCheapRemoteCost = 110
expensiveRemoteCost :: Int
expensiveRemoteCost = 200

{- Adjust's a remote's cost to reflect it being encrypted. -}
encryptedRemoteCostAdj :: Int
encryptedRemoteCostAdj = 50

{- Make sure the remote cost numbers work out. -}
prop_cost_sane :: Bool
prop_cost_sane = False `notElem`
	[ expensiveRemoteCost > 0
	, cheapRemoteCost < semiCheapRemoteCost
	, semiCheapRemoteCost < expensiveRemoteCost
	, cheapRemoteCost + encryptedRemoteCostAdj > semiCheapRemoteCost
	, cheapRemoteCost + encryptedRemoteCostAdj < expensiveRemoteCost
	, semiCheapRemoteCost + encryptedRemoteCostAdj < expensiveRemoteCost
	]

{- Checks if a repo should be ignored, based either on annex-ignore
 - setting, or on command-line options. Allows command-line to override
 - annex-ignore. -}
remoteNotIgnored :: Git.Repo -> Annex Bool
remoteNotIgnored r = do
	ignored <- getConfig r "ignore" "false"
	to <- match Annex.toremote
	from <- match Annex.fromremote
	if to || from
		then return True
		else return $ not $ Git.configTrue ignored
	where
		match a = do
			n <- Annex.getState a
			return $ n == Git.repoRemoteName r

{- If a value is specified, it is used; otherwise the default is looked up
 - in git config. forcenumcopies overrides everything. -}
getNumCopies :: Maybe Int -> Annex Int
getNumCopies v = 
	Annex.getState Annex.forcenumcopies >>= maybe (use v) (return . id)
	where
		use (Just n) = return n
		use Nothing = do
			g <- Annex.gitRepo
			return $ read $ Git.configGet g config "1"
		config = "annex.numcopies"

