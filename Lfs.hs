{-# LANGUAGE DeriveGeneric, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Client

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

data GitRef = GitRef
	{ name :: T.Text }
	deriving (Generic, Show)

instance FromJSON GitRef
instance ToJSON GitRef

type SHA256 = T.Text

-- | Adds necessary headers to a Request and makes it post the 
-- specified TransferRequest.
--
-- The input Request's url should be the discovered LFS endpoint.
-- Since this uses the LFS batch API, it adds /objects/batch to the end of
-- that url.
startTransferRequest :: Request -> TransferRequest -> Request
startTransferRequest r tr = addLfsJsonHeaders $ r
	{ path = path r <> "/objects/batch"
	, method = "POST"
	, requestBody = RequestBodyLBS (encode tr)
	}

addLfsJsonHeaders :: Request -> Request
addLfsJsonHeaders r = r
	{ requestHeaders = 
		[ ("Accept", lfsjson)
		, ("Content-Type", lfsjson)
		]
	}
  where
	lfsjson = "application/vnd.git-lfs+json"

type ParsedTransferResponse op =
	Either (Either String TransferResponseError) (TransferResponse op)

-- | Parse the body of a response to a transfer request.
parseResponseBody
	:: IsTransferResponseOperation op
	=> L.ByteString
	-> ParsedTransferResponse op
parseResponseBody resp = case eitherDecode resp of
	-- If unable to decode as a TransferResponse, try to decode
	-- as a TransferResponseError instead, in case the LFS server
	-- sent an error message.
	Left err -> case eitherDecode resp of
		Right responseerror -> Left (Right responseerror)
		Left _ -> Left $ Left err
	Right tr -> Right tr

-- | Builds a http request to perform a download.
downloadOperationRequest :: DownloadOperation -> Maybe Request
downloadOperationRequest = operationParamsRequest . download

-- | Builds http request to perform an upload. The content to upload is
-- provided in the RequestBody, along with its SHA256 and size.
--
-- If the LFS server requested verification, there will be a second
-- Request that does that; it should be run only after the upload has
-- succeeded.
uploadOperation :: UploadOperation -> RequestBody -> SHA256 -> Integer -> Maybe [Request]
uploadOperation op content oid size = 
	case (mkdlreq, mkverifyreq) of
		(Nothing, _) -> Nothing
		(Just dlreq, Nothing) -> Just [dlreq]
		(Just dlreq, Just verifyreq) -> Just [dlreq, verifyreq]
  where
	mkdlreq = mkdlreq'
		<$> operationParamsRequest (upload op)
	mkdlreq' r = r
		{ method = "PUT"
		, requestBody = content
		}
	mkverifyreq = mkverifyreq'
		<$> (operationParamsRequest =<< verify op)
	mkverifyreq' r = addLfsJsonHeaders $ r
		{ method = "POST"
		, requestBody = RequestBodyLBS $ encode $
			VerifyBody oid size
		}

data VerifyBody = VerifyBody
	{ verifybody_oid :: SHA256
	, verifybody_size :: Integer
	}
	deriving (Generic, Show)

instance ToJSON VerifyBody where
	toJSON = genericToJSON verifyBodyOptions
	toEncoding = genericToEncoding verifyBodyOptions

instance FromJSON VerifyBody where
	parseJSON = genericParseJSON verifyBodyOptions

verifyBodyOptions :: Options
verifyBodyOptions = stripFieldPrefix defaultOptions

operationParamsRequest :: OperationParams -> Maybe Request
operationParamsRequest ps = do
	r <- parseRequest (T.unpack (href ps))
	let headers = map convheader $ maybe [] M.toList (header ps)
	return $ r { requestHeaders = headers }
  where
	convheader (k, v) = (CI.mk (E.encodeUtf8 k), E.encodeUtf8 v)

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
