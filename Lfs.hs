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
transferRequestOptions = nonNullOptions
	-- remove "req_"
	{ fieldLabelModifier = drop 4 }

data TransferRequestObject = TransferRequestObject
	{ oid :: SHA256
	, size :: Integer
	}
	deriving (Generic, Show)

instance ToJSON TransferRequestObject
instance FromJSON TransferRequestObject

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

data TransferResponseError = TransferResponseError
	{ message :: T.Text
	, request_id :: Maybe T.Text
	, documentation_url :: Maybe Url
	}
	deriving (Generic, Show)

instance ToJSON TransferResponseError where
	toJSON = genericToJSON nonNullOptions
	toEncoding = genericToEncoding nonNullOptions

instance FromJSON TransferResponseError

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
	, resp_actions :: op
	}
	deriving (Generic, Show)

instance ToJSON op => ToJSON (TransferResponseOperation op) where
	toJSON = genericToJSON transferResponseOperationOptions
	toEncoding = genericToEncoding transferResponseOperationOptions

instance FromJSON op => FromJSON (TransferResponseOperation op) where
	parseJSON = genericParseJSON transferResponseOperationOptions

transferResponseOperationOptions :: Options
transferResponseOperationOptions = nonNullOptions
	-- remove "resp_"
	{ fieldLabelModifier = drop 5 }

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
	{ upload :: OperationParams }
	deriving (Generic, Show)

instance IsTransferResponseOperation UploadOperation
instance FromJSON UploadOperation
instance ToJSON UploadOperation

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
transferRequest :: Request -> TransferRequest -> Request
transferRequest r tr = r
	{ path = path r <> "/objects/batch"
	, requestHeaders =
		[ ("Accept", lfsjson)
		, ("Content-Type", lfsjson)
		]
	, method = "POST"
	, requestBody = RequestBodyLBS (encode tr)
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

-- | Builds http requests that can be used to download the objects that
-- were requested using a TransferRequest.
downloadRequests :: TransferResponse DownloadOperation -> ([(TransferResponseOperation DownloadOperation, Maybe Request)])
downloadRequests = transferRequests download

-- | Builds http requests that can be used to upload objects.
--
-- When the server already has an object, no request will be generated.
--
-- The requestBody is not set here. When making a request,
-- the content of the object needs to be provided as the body.
uploadRequests :: TransferResponse UploadOperation -> ([(TransferResponseOperation UploadOperation, Maybe Request)])
uploadRequests = transferRequests upload

transferRequests :: (op -> OperationParams) -> TransferResponse op -> ([(TransferResponseOperation op, Maybe Request)])
transferRequests getps = map mkreq . objects
  where
	mkreq op = (op, mkreq' (getps (resp_actions op)))
	mkreq' ps = do
		r <- parseRequest (T.unpack (href ps))
		let headers = map convheader $ maybe [] M.toList (header ps)
		return $ r { requestHeaders = headers }
	convheader (k, v) = (CI.mk (E.encodeUtf8 k), E.encodeUtf8 v)

type Url = T.Text

type NumSeconds = Integer

type HTTPHeader = T.Text

type HTTPHeaderValue = T.Text

-- Prevent Nothing from serializing to null.
nonNullOptions :: Options
nonNullOptions = defaultOptions { omitNothingFields = True }
