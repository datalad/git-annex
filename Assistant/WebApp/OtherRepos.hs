{- git-annex assistant webapp switching to other repos
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.OtherRepos where

import Assistant.Common
import Assistant.WebApp.Types
import qualified Git.Construct
import qualified Git.Config
import Locations.UserConfig
import qualified Utility.Url as Url

import Yesod
import Control.Concurrent
import System.Process (cwd)

{- Starts up the assistant in the repository, and waits for it to create
 - a gitAnnexUrlFile. Waits for the assistant to be up and listening for
 - connections by testing the url. Once it's running, redirect to it.
 -}
getSwitchToRepositoryR :: FilePath -> Handler RepHtml
getSwitchToRepositoryR repo = do
	liftIO startassistant
	url <- liftIO geturl
	redirect url
  where
	startassistant = do
		program <- readProgramFile
		void $ forkIO $ void $ createProcess $
			(proc program ["assistant"]) { cwd = Just repo }
	geturl = do
		r <- Git.Config.read =<< Git.Construct.fromPath repo
		waiturl $ gitAnnexUrlFile r
	waiturl urlfile = do
		v <- tryIO $ readFile urlfile
		case v of
			Left _ -> delayed $ waiturl urlfile
			Right url -> ifM (listening url)
				( return url
				, delayed $ waiturl urlfile
				)
	listening url = catchBoolIO $ fst <$> Url.exists url []
	delayed a = do
		threadDelay 100000 -- 1/10th of a second
		a
