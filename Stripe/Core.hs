{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings #-}
module Stripe.Core where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Data
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (mconcat)
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.String   (fromString)
import Data.Text (Text)

toStrict :: BL.ByteString -> ByteString
toStrict = mconcat . BL.toChunks

-- WARNING: only safe on ascii
showBS :: (Show a) => a -> ByteString
showBS = fromString . show

boolBS :: Bool -> ByteString
boolBS True  = "true"
boolBS False = "false"

mbParam :: ByteString -> Maybe a -> (a -> ByteString) -> Maybe (ByteString, ByteString)
mbParam _ Nothing _ = Nothing
mbParam name (Just v) show' = Just (name, show' v)

data SMethod
    = SGet
    | SPost [(ByteString, ByteString)]
    | SDelete
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''SMethod)

data StripeReq ret = StripeReq
    { srUrl         :: String
    , srQueryString :: [(ByteString, ByteString)]
    , srMethod      :: SMethod
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeReq)

type Currency = Text

usd :: Currency
usd = "usd"

type Timestamp = Integer    
type Count  = Integer
type Offset = Integer

data List a = List
    { count :: Integer
    , data_ :: [a]
    }

instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) =
        List <$> obj .: "count"
             <*> obj .: "data"
    parseJSON _ = mzero

newtype ApiKey = ApiKey { unApiKey :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CardError
    = IncorrectNumber
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

instance FromJSON CardError where
    parseJSON (String str)
        | str == "incorrect_number"     = return IncorrectNumber
        | str == "invalid_expiry_month" = return InvalidExpiryMonth
        | str == "invalid_expiry_year"  = return InvalidExpiryYear
        | str == "invalid_cvc"          = return InvalidCvc
        | str == "expired_card"         = return ExpiredCard
        | str == "incorrect_cvc"        = return IncorrectCvc
        | str == "card_declined"        = return CardDeclined
        | str == "missing"              = return Missing
        | str == "processing_error"     = return ProcessingError
        | otherwise                     = mzero
    parseJSON _ = mzero

data ErrorType
    = InvalidRequestError
    | ApiError
    | CardError
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ErrorType)

instance FromJSON ErrorType where
    parseJSON (String str)
        | str == "card_error" = return CardError
        | str == "invalid_request_error" = return InvalidRequestError
        | str == "api_error"  = return ApiError
    parseJSON _ = mzero

data StripeError = StripeError
    { errorType :: ErrorType
    , message   :: Text
    , code      :: Maybe CardError
    , param     :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeError)

instance FromJSON StripeError where
    parseJSON (Object obj') =
        case HashMap.lookup "error" obj' of
          (Just (Object obj)) ->
              StripeError <$> obj .: "type"
                          <*> obj .: "message"
                          <*> obj .:? "code"
                          <*> obj .:? "param"
          _ -> mzero
    parseJSON _ = mzero

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
    , cardAddrCity       :: Maybe Text
    , cardAddrCountry    :: Maybe Text
    , cardAddrLine1      :: Maybe Text
    , cardAddrLine1Check :: Maybe Check
    , cardAddrLine2      :: Maybe Text
    , cardAddrState      :: Maybe Text
    , cardAddrZip        :: Maybe Text
    , cardAddrZipCheck   :: Maybe Check
    , cardCountry        :: Maybe Text
    , cardCvcCheck       :: Maybe Check
    , cardName           :: Maybe Text
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

type Cents = Integer

newtype CustomerId = CustomerId { unCustomerId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

