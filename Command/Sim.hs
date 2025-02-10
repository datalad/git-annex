{- git-annex command
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.Sim where

import Command
import Annex.Sim
import Annex.Sim.File
import Annex.Perms

import System.Random

cmd :: Command
cmd = command "sim" SectionTesting
	"simulate a network of repositories"
	paramCommand (withParams seek)

seek :: CmdParams -> CommandSeek
seek ("start":[]) = startsim Nothing
seek ("start":simfile:[]) = startsim (Just simfile)
seek ("end":[]) = endsim
seek ("show":[]) = do
	simdir <- fromRepo gitAnnexSimDir
	liftIO (restoreSim simdir) >>= \case
		Left err -> giveup err
		Right st -> showsim st
seek ("run":simfile:[]) = startsim' (Just simfile) >>= cleanup
  where
	cleanup st = do 
		st' <- liftIO $ quiesceSim st
		endsim
		when (simFailed st') $ do
			showsim st'
			giveup "Simulation shown above had errors."
seek ps = case parseSimCommand ps of
	Left err -> giveup err
	Right simcmd -> do
		repobyname <- mkGetExistingRepoByName
		simdir <- fromRepo gitAnnexSimDir
		liftIO (restoreSim simdir) >>= \case
			Left err -> giveup err
			Right st -> do
				st' <- runSimCommand simcmd repobyname st
				liftIO $ suspendSim st'
				when (simFailed st' && not (simFailed st)) $
					giveup "Simulation had errors."

startsim :: Maybe FilePath -> CommandSeek
startsim simfile = startsim' simfile >>= cleanup
  where
	cleanup st = do
		liftIO $ suspendSim st
		when (simFailed st) $
			giveup "Simulation had errors."

startsim' :: Maybe FilePath -> Annex (SimState SimRepo)
startsim' simfile = do
	simdir <- fromRepo gitAnnexSimDir
	whenM (liftIO $ doesDirectoryExist simdir) $
		giveup "A sim was previously started. Use `git-annex sim end` to stop it before starting a new one."
	
	showLongNote $ UnquotedString "Sim started."
	rng <- liftIO $ fst . random <$> getStdGen
	let st = emptySimState rng (fromOsPath simdir)
	case simfile of
		Nothing -> startup simdir st []
		Just f -> liftIO (readFile f) >>= \c -> 
			case parseSimFile c of
				Left err -> giveup err
				Right cs -> startup simdir st cs
  where
	startup simdir st cs = do
		repobyname <- mkGetExistingRepoByName
		createAnnexDirectory simdir
		let st' = recordSeed st cs
		go st' repobyname cs

	go st _ [] = return st
	go st repobyname (c:cs) = do
		st' <- runSimCommand c repobyname st
		go st' repobyname cs
	
endsim :: CommandSeek
endsim = do
	simdir <- fromRepo gitAnnexSimDir
	whenM (liftIO $ doesDirectoryExist simdir) $ do
		liftIO $ removeDirectoryRecursive simdir
	showLongNote $ UnquotedString "Sim ended."
		
showsim :: SimState SimRepo -> Annex ()
showsim = liftIO . putStr . generateSimFile . reverse . simHistory
