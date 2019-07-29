{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T

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
	{ transfer :: TransferAdapter
	, objects :: [TransferResponseOperation op]
	}
	deriving (Generic, Show)

instance ToJSON (TransferResponse DownloadOperation)
instance FromJSON (TransferResponse DownloadOperation)
instance ToJSON (TransferResponse UploadOperation)
instance FromJSON (TransferResponse UploadOperation)

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
	, resp_authenticated :: Bool
	, resp_actions :: op
	}
	deriving (Generic, Show)

instance ToJSON op => ToJSON (TransferResponseOperation op) where
	toJSON = genericToJSON transferResponseOperationOptions
	toEncoding = genericToEncoding transferResponseOperationOptions

instance FromJSON op => FromJSON (TransferResponseOperation op) where
	parseJSON = genericParseJSON transferResponseOperationOptions

transferResponseOperationOptions :: Options
transferResponseOperationOptions = defaultOptions
	-- remove "resp_"
	{ fieldLabelModifier = drop 5 }

data DownloadOperation = DownloadOperation
	{ download :: OperationParams }
	deriving (Generic, Show)

instance ToJSON DownloadOperation
instance FromJSON DownloadOperation

data UploadOperation = UploadOperation
	{ upload :: OperationParams }
	deriving (Generic, Show)

instance FromJSON UploadOperation
instance ToJSON UploadOperation

data OperationParams = OperationParams
	{ href :: Url
	, header :: M.Map HTTPHeader HTTPHeaderValue
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

type Url = T.Text

type NumSeconds = Integer

type HTTPHeader = T.Text

type HTTPHeaderValue = T.Text

-- Prevent Nothing from serializing to null.
nonNullOptions :: Options
nonNullOptions = defaultOptions { omitNothingFields = True }


