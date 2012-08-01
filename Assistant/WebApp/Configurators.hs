{- git-annex assistant webapp configurators
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators where

import Assistant.Common
import Assistant.WebApp
import Assistant.WebApp.SideBar
import Utility.Yesod
import qualified Remote
import Logs.Web (webUUID)
import Logs.Trust
import Annex.UUID (getUUID)

import Yesod
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

{- An intro message, list of repositories, and nudge to make more. -}
introDisplay :: Text -> Widget
introDisplay ident = do
	webapp <- lift getYesod
	l <- lift $ runAnnex [] $ do
		u <- getUUID
		rs <- map Remote.uuid <$> Remote.remoteList
		rs' <- snd <$> trustPartition DeadTrusted rs
		Remote.prettyListUUIDs $ filter (/= webUUID) $ nub $ u:rs'
	let remotelist = zip counter l
	let n = length l
	let numrepos = show n
	let notenough = n < 2
	let barelyenough = n == 2
	let morethanenough = n > 2
	$(widgetFile "configurators/intro")
	lift $ modifyWebAppState $ \s -> s { showIntro = False }
	where
		counter = map show ([1..] :: [Int])

data RepositoryPath = RepositoryPath Text
	deriving Show

{- Custom field display for a RepositoryPath, with an icon etc.
 -
 - Validates that the path entered is not empty, and is a safe value
 - to use as a repository. -}
repositoryPathField :: forall sub. Bool -> Field sub WebApp Text
repositoryPathField autofocus = Field { fieldParse = parse, fieldView = view }
	where
		view idAttr nameAttr attrs val isReq =
			[whamlet|<input type="text" *{attrs} id="#{idAttr}" name="#{nameAttr}" :isReq:required :autofocus:autofocus value="#{either id id val}">|]

		parse [path]
			| T.null path = nopath
			| otherwise = liftIO $ checkRepositoryPath path
		parse [] = return $ Right Nothing
		parse _ = nopath

		nopath = return $ Left "Enter a location for the repository"

{- As well as checking the path for a lot of silly things, tilde is
 - expanded in the returned path. -}
checkRepositoryPath :: Text -> IO (Either (SomeMessage WebApp) (Maybe Text))
checkRepositoryPath p = do
	home <- myHomeDir
	let basepath = expandTilde home $ T.unpack p
	path <- absPath basepath
	let parent = parentDir path
	problems <- catMaybes <$> mapM runcheck
		[ (return $ path == "/", "Enter the full path to use for the repository.")
		, (return $ all isSpace basepath, "A blank path? Seems unlikely.")
		, (doesFileExist path, "A file already exists with that name.")
		, (return $ path == home, "Sorry, using git-annex for your whole home directory is not currently supported.")
		, (not <$> doesDirectoryExist parent, "Parent directory does not exist.")
		, (cannotWrite path, "Cannot write a repository there.")
		]
	return $ 
		case headMaybe problems of
			Nothing -> Right $ Just $ T.pack basepath
			Just prob -> Left prob
	where
		runcheck (chk, msg) = ifM (chk)
			( return $ Just msg
			, return Nothing
			)
		cannotWrite path = do
			tocheck <- ifM (doesDirectoryExist path)
				(return path, return $ parentDir path)
			not <$> (catchBoolIO $ fileAccess tocheck False True False)
		expandTilde home ('~':path) = home </> path
		expandTilde _ path = path

{- If run in the home directory, default to putting it in ~/Desktop/annex,
 - when a Desktop directory exists, and ~/annex otherwise.
 -
 - If run in another directory, the user probably wants to put it there. -}
defaultRepositoryPath :: IO FilePath
defaultRepositoryPath = do
	cwd <- liftIO $ getCurrentDirectory
	home <- myHomeDir
	if home == cwd
		then ifM (doesDirectoryExist $ home </> "Desktop")
			(return "~/Desktop/annex", return "~")
		else return cwd

addRepositoryForm :: Form RepositoryPath
addRepositoryForm msg = do
	path <- T.pack . addTrailingPathSeparator
		<$> liftIO defaultRepositoryPath
	(pathRes, pathView) <- mreq (repositoryPathField True) ""(Just path)
	let (err, errmsg) = case pathRes of
		FormMissing -> (False, "")
		FormFailure l -> (True, concat $ map T.unpack l)
		FormSuccess _ -> (False, "")
	let form = do
		webAppFormAuthToken
		$(widgetFile "configurators/addrepository/form")
	return (RepositoryPath <$> pathRes, form)

addRepository :: Bool -> Widget
addRepository firstrun = do
	setTitle $ if firstrun then "Getting started" else "Add repository"
	((res, form), enctype) <- lift $ runFormGet addRepositoryForm
	case res of
		FormSuccess (RepositoryPath p) -> error $ "TODO" ++ show p
		_ -> $(widgetFile "configurators/addrepository")

getAddRepositoryR :: Handler RepHtml
getAddRepositoryR = bootstrap (Just Config) $ do
	sideBarDisplay
	addRepository False

getConfigR :: Handler RepHtml
getConfigR = bootstrap (Just Config) $ do
	sideBarDisplay
	ifM (lift inFirstRun)
		( addRepository True
		, do
			setTitle "Configuration"
			$(widgetFile "configurators/main")
		)
