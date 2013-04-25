{- git-annex assistant webapp configurators for Internet Archive
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings, RankNTypes #-}

module Assistant.WebApp.Configurators.IA where

import Assistant.WebApp.Common
import qualified Assistant.WebApp.Configurators.AWS as AWS
#ifdef WITH_S3
import qualified Remote.S3 as S3
#endif
import qualified Remote
import Types.StandardGroups
import Logs.PreferredContent

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char
import Network.Socket (HostName)
import Network.URI

iaConfigurator :: Widget -> Handler RepHtml
iaConfigurator = page "Add an Internet Archive repository" (Just Configuration)

data IAInput = IAInput
	{ accessKeyID :: Text
	, secretAccessKey :: Text
	, mediaType :: MediaType
	, itemDescription :: Text
	}

extractCreds :: IAInput -> AWS.AWSCreds
extractCreds i = AWS.AWSCreds (accessKeyID i) (secretAccessKey i)

{- IA defines only a few media types currently, or the media type
 - may be omitted
 -
 - We add a few other common types, mapped to what we've been told
 - is the closest match.
 -}
data MediaType = MediaImages | MediaAudio | MediaMovies | MediaText | MediaSoftware | MediaOmitted
	deriving (Eq, Ord, Enum, Bounded)

{- Format a MediaType for entry into the IA metadata -}
formatMediaType :: MediaType -> String
formatMediaType MediaText = "texts"
formatMediaType MediaImages = "image"
formatMediaType MediaSoftware = "software"
formatMediaType MediaMovies = "movies"
formatMediaType MediaAudio = "audio"
formatMediaType MediaOmitted = ""

{- A default collection to use for each Mediatype, where one is known. -}
collectionMediaType :: MediaType -> Maybe String
collectionMediaType MediaText = Just "opensource"
collectionMediaType MediaImages = Just "ourmedia"
collectionMediaType MediaSoftware = Nothing
collectionMediaType MediaMovies = Just "opensource_movies"
collectionMediaType MediaAudio = Just "opensource_audio"
collectionMediaType MediaOmitted = Nothing

{- Format a MediaType for user display. -}
showMediaType :: MediaType -> String
showMediaType MediaText = "texts"
showMediaType MediaImages = "photos & images"
showMediaType MediaSoftware = "software"
showMediaType MediaMovies = "videos & movies"
showMediaType MediaAudio = "audio & music"
showMediaType MediaOmitted = "other"

iaInputAForm :: AForm WebApp WebApp IAInput
iaInputAForm = IAInput
	<$> accessKeyIDFieldWithHelp
	<*> AWS.secretAccessKeyField
	<*> areq (selectFieldList mediatypes) "Media Type" (Just MediaOmitted)
	<*> areq textField "Description" Nothing
  where
	mediatypes :: [(Text, MediaType)]
	mediatypes = map (\t -> (T.pack $ showMediaType t, t)) [minBound..]

accessKeyIDFieldWithHelp :: AForm WebApp WebApp Text
accessKeyIDFieldWithHelp = AWS.accessKeyIDField help
  where
	help = [whamlet|
<a href="http://archive.org/account/s3.php">
  Get Internet Archive access keys
|]

getAddIAR :: Handler RepHtml
getAddIAR = postAddIAR

postAddIAR :: Handler RepHtml
#ifdef WITH_S3
postAddIAR = iaConfigurator $ do
	((result, form), enctype) <- lift $
		runFormPost $ renderBootstrap iaInputAForm
	case result of
		FormSuccess input -> lift $ do
			let name = escapeBucket $ T.unpack $ itemDescription input
			AWS.makeAWSRemote S3.remote (extractCreds input) name setgroup $
				M.fromList $ catMaybes
					[ Just $ configureEncryption NoEncryption
					, Just ("type", "S3")
					, Just ("host", iaHost)
					, Just ("bucket", escapeHeader name)
					, Just ("x-archive-meta-title", escapeHeader $ T.unpack $ itemDescription input)
					, if mediaType input == MediaOmitted
						then Nothing
						else Just ("x-archive-mediatype", formatMediaType $ mediaType input)
					, (,) <$> pure "x-archive-meta-collection" <*> collectionMediaType (mediaType input)
					-- Make item show up ASAP.
					, Just ("x-archive-interactive-priority", "1")
					]
		_ -> $(widgetFile "configurators/addia")
  where
	setgroup r = liftAnnex $
		setStandardGroup (Remote.uuid r) PublicGroup
#else
postAddIAR = error "S3 not supported by this build"
#endif

getEnableIAR :: UUID -> Handler RepHtml
getEnableIAR = postEnableIAR

postEnableIAR :: UUID -> Handler RepHtml
#ifdef WITH_S3
postEnableIAR = iaConfigurator . AWS.enableAWSRemote S3.remote
#else
postEnableIAR _ = error "S3 not supported by this build"
#endif

{- Convert a description into a bucket name, which will also be
 - used as the repository name.
 - IA seems to need only lower case, and no spaces. -}
escapeBucket :: String -> String
escapeBucket = map toLower . replace " " "-"

{- IA S3 API likes headers to be URI escaped, escaping spaces looks ugly. -}
escapeHeader :: String -> String
escapeHeader = escapeURIString (\c -> isUnescapedInURI c && c /= ' ')

iaHost :: HostName
iaHost = "s3.us.archive.org"
