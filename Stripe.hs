{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe where

import Data.ByteString (ByteString)
import Data.Aeson
import Data.Conduit
import Data.Data     (Data, Typeable)
import Data.Maybe
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.Text     (Text)
import Network.HTTP.Conduit

newtype ApiKey = ApiKey { unApiKey :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CardError
    = IncorrectCardNumber
    | InvalidExpiryMonth
    | InvalidExpiryYear
    | InvalidCvc
    | ExpiredCard
    | IncorrectCvc
    | CardDeclined
    | Missing
    | ProcessingError
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardError)

data ErrorType
    = InvalidRequestError
    | ApiError
    | CardError CardError
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ErrorType)

data StripeError = StripeError
    { errorType :: ErrorType
    , message   :: Text
    , param     :: Maybe Text
    }
$(deriveSafeCopy 0 'base ''StripeError)

charges :: Request m
charges = fromJust $ parseUrl "https://api.stripe.com/v1/charges"

-- stripe :: ApiKey -> Request m -> IO 
stripe (ApiKey k) req manager =
    do res <- httpLbs (applyBasicAuth k "" req) manager
       return res
