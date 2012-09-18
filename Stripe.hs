{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe where

import Control.Applicative ((<$>), (<*>))
import Control.Monad        (mzero)
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
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeError)

newtype CustomerId = CustomerId { unCustomerId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data Check
    = Pass
    | Fail
    | Unchecked
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Check)

instance FromJSON Check where
    parseJSON (String str)
        | str == "pass"      = return Pass
        | str == "fail"      = return Fail
        | str == "unchecked" = return Unchecked
        | otherwise          = mzero
    parseJSON Null = return Unchecked
    parseJSON _    = mzero

data Card = Card
    { cardExpMonth       :: Int
    , cardExpYear        :: Int
    , cardFingerprint    :: Text
    , cardLast4          :: Text
    , cardType           :: Text
    , cardAddrCity       :: Text
    , cardAddrCountry    :: Text
    , cardAddrLine1      :: Text
    , cardAddrLine1Check :: Check
    , cardAddrLine2      :: Text
    , cardAddrState      :: Text
    , cardAddrZip        :: Text
    , cardAddrZipCheck   :: Check
    , cardCountry        :: Text
    , cardCvcCheck       :: Check
    , cardName           :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Card)

instance FromJSON Card where
    parseJSON (Object obj) =
        Card <$> obj .: "exp_month"
             <*> obj .: "exp_year"
             <*> obj .: "fingerprint"
             <*> obj .: "last4"
             <*> obj .: "type"
             <*> obj .: "address_city"
             <*> obj .: "address_country"
             <*> obj .: "address_line1"
             <*> obj .: "address_line1_check"
             <*> obj .: "address_line2"
             <*> obj .: "address_state"
             <*> obj .: "address_zip"
             <*> obj .: "address_zip_check"
             <*> obj .: "country"
             <*> obj .: "cvc_check"
             <*> obj .: "name"
    parseJSON _ = mzero
             


{-
data Charges = Charges
    { 
    }
-}
type Count = Integer
type Offset = Integer

charges :: Maybe Count -> Maybe Offset -> Maybe CustomerId -> Request m
charges mCount mOffset mCustomerId =
    fromJust $ parseUrl "https://api.stripe.com/v1/charges"

-- stripe :: ApiKey -> Request m -> IO 
stripe (ApiKey k) req manager =
    do res <- httpLbs (applyBasicAuth k "" req) manager
       return res
