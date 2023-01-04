{- git-lfs API
 - 
 - https://github.com/git-lfs/git-lfs/blob/master/docs/api
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

-- | This implementation of the git-lfs API uses http Request and Response,
-- but leaves actually connecting up the http client to the user.
--
-- You'll want to use a Manager that supports https, since the protocol
-- uses http basic auth.
--
-- Some LFS servers, notably Github's, may require a User-Agent header
-- in some of the requests, in order to allow eg, uploads. No such header
-- is added by default, so be sure to add your own.

{-# LANGUAGE DeriveGeneric, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- This is a vendored copy of Network.GitLFS from the git-lfs package,
-- and will be removed once that package is available in all build
-- environments.
module Utility.GitLFS (
	-- * Transfer requests
	TransferRequest(..),
	TransferRequestOperation(..),
	TransferAdapter(..),
	TransferRequestObject(..),
	startTransferRequest,

	-- * Responses to transfer requests
	TransferResponse(..),
	TransferResponseOperation(..),
	IsTransferResponseOperation,
	DownloadOperation(..),
	UploadOperation(..),
	OperationParams(..),
	ParsedTransferResponse(..),
	parseTransferResponse,

	-- * Making transfers
	downloadOperationRequest,
	uploadOperationRequests,
	ServerSupportsChunks(..),

	-- * Endpoint discovery
	Endpoint,
	guessEndpoint,
	modifyEndpointRequest,
	sshDiscoverEndpointCommand,
	parseSshDiscoverEndpointResponse,

	-- * Errors
	TransferResponseError(..),
	TransferResponseObjectError(..),

	-- * Additional data types
	Url,
	SHA256,
	GitRef(..),
	NumSeconds,
	HTTPHeader,
	HTTPHeaderValue,
) where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Network.HTTP.Client
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import qualified Network.URI as URI

data TransferRequest = TransferRequest
	{ req_operation :: TransferRequestOperation
	, req_transfers :: [TransferAdapter]
	, req_ref :: Maybe GitRef
	, req_objects :: [TransferRequestObject]
	}
	deriving (Generic, Show)

instance ToJSON TransferRequest where
	toJSON = genericToJSON transferRequestOptions
	toEncoding = genericToEncoding transferRequestOptions

instance FromJSON TransferRequest where
	parseJSON = genericParseJSON transferRequestOptions

transferRequestOptions :: Options
transferRequestOptions = stripFieldPrefix nonNullOptions

data TransferRequestObject = TransferRequestObject
	{ req_oid :: SHA256
	, req_size :: Integer
	}
	deriving (Generic, Show)

instance ToJSON TransferRequestObject where
	toJSON = genericToJSON transferRequestObjectOptions
	toEncoding = genericToEncoding transferRequestObjectOptions

instance FromJSON TransferRequestObject where
	parseJSON = genericParseJSON transferRequestObjectOptions

transferRequestObjectOptions :: Options
transferRequestObjectOptions = stripFieldPrefix defaultOptions

data TransferRequestOperation = RequestDownload | RequestUpload
	deriving (Show)

instance ToJSON TransferRequestOperation where
	toJSON RequestDownload = "download"
	toJSON RequestUpload = "upload"

instance FromJSON TransferRequestOperation where
	parseJSON (String "download") = pure RequestDownload
	parseJSON (String "upload") = pure RequestUpload
	parseJSON invalid = typeMismatch "TransferRequestOperation" invalid

data TransferResponse op = TransferResponse
	{ transfer :: Maybe TransferAdapter
	, objects :: [TransferResponseOperation op]
	}
	deriving (Generic, Show)

instance IsTransferResponseOperation op => ToJSON (TransferResponse op) where
	toJSON = genericToJSON nonNullOptions
	toEncoding = genericToEncoding nonNullOptions

instance IsTransferResponseOperation op => FromJSON (TransferResponse op)

-- | This is an error with a TransferRequest as a whole. It's also possible
-- for a TransferRequest to overall succeed, but fail for some
-- objects; such failures use TransferResponseObjectError.
data TransferResponseError = TransferResponseError
	{ resperr_message :: T.Text
	, resperr_request_id :: Maybe T.Text
	, resperr_documentation_url :: Maybe Url
	}
	deriving (Generic, Show)

instance ToJSON TransferResponseError where
	toJSON = genericToJSON transferResponseErrorOptions
	toEncoding = genericToEncoding transferResponseErrorOptions

instance FromJSON TransferResponseError where
	parseJSON = genericParseJSON transferResponseErrorOptions

transferResponseErrorOptions :: Options
transferResponseErrorOptions = stripFieldPrefix nonNullOptions

-- | An error with a single object within a TransferRequest.
data TransferResponseObjectError = TransferResponseObjectError
	{ respobjerr_code :: Int
	, respobjerr_message :: T.Text
	}
	deriving (Generic, Show)

instance ToJSON TransferResponseObjectError where
	toJSON = genericToJSON transferResponseObjectErrorOptions
	toEncoding = genericToEncoding transferResponseObjectErrorOptions

instance FromJSON TransferResponseObjectError where
	parseJSON = genericParseJSON transferResponseObjectErrorOptions

transferResponseObjectErrorOptions :: Options
transferResponseObjectErrorOptions = stripFieldPrefix nonNullOptions

data TransferAdapter = Basic
	deriving (Show)

instance ToJSON TransferAdapter where
	toJSON Basic = "basic"

instance FromJSON TransferAdapter where
	parseJSON (String "basic") = pure Basic
	parseJSON invalid = typeMismatch "basic" invalid

data TransferResponseOperation op = TransferResponseOperation
	{ resp_oid :: SHA256
	, resp_size :: Integer
	, resp_authenticated :: Maybe Bool
	, resp_actions :: Maybe op
	, resp_error :: Maybe TransferResponseObjectError
	}
	deriving (Generic, Show)

instance ToJSON op => ToJSON (TransferResponseOperation op) where
	toJSON = genericToJSON transferResponseOperationOptions
	toEncoding = genericToEncoding transferResponseOperationOptions

instance FromJSON op => FromJSON (TransferResponseOperation op) where
	parseJSON = genericParseJSON transferResponseOperationOptions

transferResponseOperationOptions :: Options
transferResponseOperationOptions = stripFieldPrefix nonNullOptions

-- | Class of types that can be responses to a transfer request,
-- that contain an operation to use to make the transfer.
class (FromJSON op, ToJSON op) => IsTransferResponseOperation op

data DownloadOperation = DownloadOperation
	{ download :: OperationParams }
	deriving (Generic, Show)

instance IsTransferResponseOperation DownloadOperation
instance ToJSON DownloadOperation
instance FromJSON DownloadOperation

data UploadOperation = UploadOperation
	{ upload :: OperationParams
	, verify :: Maybe OperationParams
	}
	deriving (Generic, Show)

instance IsTransferResponseOperation UploadOperation

instance ToJSON UploadOperation where
	toJSON = genericToJSON nonNullOptions
	toEncoding = genericToEncoding nonNullOptions

instance FromJSON UploadOperation

data OperationParams = OperationParams
	{ href :: Url
	, header :: Maybe (M.Map HTTPHeader HTTPHeaderValue)
	, expires_in :: Maybe NumSeconds
	, expires_at :: Maybe T.Text
	}
	deriving (Generic, Show)

instance ToJSON OperationParams where
	toJSON = genericToJSON nonNullOptions
	toEncoding = genericToEncoding nonNullOptions

instance FromJSON OperationParams

data Verification = Verification
	{ verification_oid :: SHA256
	, verification_size :: Integer
	}
	deriving (Generic, Show)

instance ToJSON Verification where
	toJSON = genericToJSON verificationOptions
	toEncoding = genericToEncoding verificationOptions

instance FromJSON Verification where
	parseJSON = genericParseJSON verificationOptions

verificationOptions :: Options
verificationOptions = stripFieldPrefix defaultOptions

-- | Sent over ssh connection when using that to find the endpoint.
data SshDiscoveryResponse = SshDiscoveryResponse
	{ endpoint_href :: Url
	, endpoint_header :: Maybe (M.Map HTTPHeader HTTPHeaderValue)
	, endpoint_expires_in :: Maybe NumSeconds
	, endpoint_expires_at :: Maybe T.Text
	} deriving (Generic, Show)

instance ToJSON SshDiscoveryResponse where
	toJSON = genericToJSON sshDiscoveryResponseOptions
	toEncoding = genericToEncoding sshDiscoveryResponseOptions

instance FromJSON SshDiscoveryResponse where
	parseJSON = genericParseJSON sshDiscoveryResponseOptions

sshDiscoveryResponseOptions :: Options
sshDiscoveryResponseOptions = stripFieldPrefix nonNullOptions

data GitRef = GitRef
	{ name :: T.Text }
	deriving (Generic, Show)

instance FromJSON GitRef
instance ToJSON GitRef

type SHA256 = T.Text

-- | The endpoint of a git-lfs server.
data Endpoint = Endpoint Request
	deriving (Show)

-- | Command to run via ssh with to discover an endpoint. The FilePath is
-- the location of the git repository on the ssh server.
--
-- Note that, when sshing to the server, you should take care that the
-- hostname you pass to ssh is really a hostname and not something that ssh
-- will parse an an option, such as -oProxyCommand=".
sshDiscoverEndpointCommand :: FilePath -> TransferRequestOperation -> [String]
sshDiscoverEndpointCommand remotepath tro =
	[ "git-lfs-authenticate"
	, remotepath
	, case tro of
		RequestDownload -> "download"
		RequestUpload -> "upload"
	]

-- Internal smart constructor for an Endpoint.
-- 
-- Since this uses the LFS batch API, it adds /objects/batch
-- to the endpoint url. It also adds the necessary headers to use JSON.
mkEndpoint :: URI.URI -> Maybe Endpoint
mkEndpoint uri = do
	r <- requestFromURI uri
	let r' = addLfsJsonHeaders $ r { path = path r <> "/objects/batch" }
	return (Endpoint r')

-- | Parse the json output when doing ssh endpoint discovery.
parseSshDiscoverEndpointResponse :: L.ByteString -> Maybe Endpoint
parseSshDiscoverEndpointResponse resp = do
	sr <- decode resp
	uri <- URI.parseURI (T.unpack (endpoint_href sr))
	endpoint <- mkEndpoint uri
	return $ modifyEndpointRequest endpoint $ case endpoint_header sr of
		Nothing -> id
		Just headers ->
			let headers' = map convheader (M.toList headers)
			in \req -> req
				{ requestHeaders = requestHeaders req ++ headers' }
  where
	convheader (k, v) = (CI.mk (E.encodeUtf8 k), E.encodeUtf8 v)

-- | Guesses the LFS endpoint from the http url of a git remote.
--
-- https://github.com/git-lfs/git-lfs/blob/master/docs/api/server-discovery.md
guessEndpoint :: URI.URI -> Maybe Endpoint
guessEndpoint uri = case URI.uriScheme uri of
	"https:" -> endpoint
	"http:" -> endpoint
	_ -> Nothing
  where
	endpoint = mkEndpoint $ uri
		-- force https because the git-lfs protocol uses http
		-- basic auth tokens, which should not be exposed
		{ URI.uriScheme = "https:"
		, URI.uriPath = guessedpath
		}
	
	guessedpath
		| ".git" `isSuffixOf` URI.uriPath uri =
			URI.uriPath uri ++ "/info/lfs"
		| ".git/" `isSuffixOf` URI.uriPath uri =
			URI.uriPath uri ++ "info/lfs"
		| otherwise = (droptrailing '/' (URI.uriPath uri)) ++ ".git/info/lfs"
	
	droptrailing c = reverse . dropWhile (== c) . reverse

-- | When an Endpoint is used to generate a Request, this allows adjusting
-- that Request.
--
-- This can be used to add http basic authentication to an Endpoint:
--
-- > modifyEndpointRequest (guessEndpoint u) (applyBasicAuth "user" "pass")
modifyEndpointRequest :: Endpoint -> (Request -> Request) -> Endpoint
modifyEndpointRequest (Endpoint r) f = Endpoint (f r)

-- | Makes a Request that will start the process of making a transfer to or
-- from the LFS endpoint.
startTransferRequest :: Endpoint -> TransferRequest -> Request
startTransferRequest (Endpoint r) tr = r
	{ method = "POST"
	, requestBody = RequestBodyLBS (encode tr)
	}

addLfsJsonHeaders :: Request -> Request
addLfsJsonHeaders r = r
	{ requestHeaders = requestHeaders r ++
		[ ("Accept", lfsjson)
		, ("Content-Type", lfsjson)
		]
	}
  where
	lfsjson = "application/vnd.git-lfs+json"

data ParsedTransferResponse op
	= ParsedTransferResponse (TransferResponse op)
	| ParsedTransferResponseError TransferResponseError
	| ParseFailed String

-- | Parse the body of a response to a transfer request.
parseTransferResponse
	:: IsTransferResponseOperation op
	=> L.ByteString
	-> ParsedTransferResponse op
parseTransferResponse resp = case eitherDecode resp of
	Right tr -> ParsedTransferResponse tr
	-- If unable to decode as a TransferResponse, try to decode
	-- as a TransferResponseError instead, in case the LFS server
	-- sent an error message.
	Left err ->
		either (const $ ParseFailed err) ParsedTransferResponseError $
			eitherDecode resp

-- | Builds a http request to perform a download.
downloadOperationRequest :: DownloadOperation -> Maybe Request
downloadOperationRequest = fmap fst . operationParamsRequest . download

-- | Builds http request to perform an upload. The content to upload is
-- provided, along with its SHA256 and size.
--
-- When the LFS server requested verification, there will be a second
-- Request that does that; it should be run only after the upload has
-- succeeded.
--
-- When the LFS server already contains the object, an empty list may be
-- returned.
uploadOperationRequests :: UploadOperation -> (ServerSupportsChunks -> RequestBody) -> SHA256 -> Integer -> Maybe [Request]
uploadOperationRequests op mkcontent oid size = 
	case (mkdlreq, mkverifyreq) of
		(Nothing, _) -> Nothing
		(Just dlreq, Nothing) -> Just [dlreq]
		(Just dlreq, Just verifyreq) -> Just [dlreq, verifyreq]
  where
	mkdlreq = mkdlreq'
		<$> operationParamsRequest (upload op)
	mkdlreq' (r, ssc) = r
		{ method = "PUT"
		, requestBody = mkcontent ssc
		}
	mkverifyreq = mkverifyreq'
		<$> (operationParamsRequest =<< verify op)
	mkverifyreq' (r, _ssc) = addLfsJsonHeaders $ r
		{ method = "POST"
		, requestBody = RequestBodyLBS $ encode $
			Verification oid size
		}

-- | When the LFS server indicates that it supports Transfer-Encoding chunked,
-- this will contain a true value, and the RequestBody provided to
-- uploadOperationRequests may be created using RequestBodyStreamChunked.
-- Otherwise, that should be avoided as the server may not support the
-- chunked encoding.
newtype ServerSupportsChunks = ServerSupportsChunks Bool

operationParamsRequest :: OperationParams -> Maybe (Request, ServerSupportsChunks)
operationParamsRequest ps = do
	r <- parseRequest (T.unpack (href ps))
	let headers = map convheader $ maybe [] M.toList (header ps)
	let headers' = filter allowedheader headers
	let ssc = ServerSupportsChunks $
		any (== ("Transfer-Encoding", "chunked")) headers
	return (r { requestHeaders = headers' }, ssc)
  where
	convheader (k, v) = (CI.mk (E.encodeUtf8 k), E.encodeUtf8 v)
	-- requestHeaders is not allowed to set Transfer-Encoding or 
	-- Content-Length; copying those over blindly could request in a
	-- malformed request.
	allowedheader (k, _) = k /= "Transfer-Encoding"
		&& k /= "Content-Length"

type Url = T.Text

type NumSeconds = Integer

type HTTPHeader = T.Text

type HTTPHeaderValue = T.Text

-- Prevent Nothing from serializing to null.
nonNullOptions :: Options
nonNullOptions = defaultOptions { omitNothingFields = True }

-- Remove prefix from field names.
stripFieldPrefix :: Options -> Options
stripFieldPrefix o =
	o { fieldLabelModifier = drop 1 . dropWhile (/= '_') }
