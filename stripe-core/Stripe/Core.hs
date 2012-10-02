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

-- | convert a lazy 'ByteString' to a strict 'ByteString'
toStrict :: BL.ByteString -> ByteString
toStrict = mconcat . BL.toChunks

-- | alias for @fromString . show@
-- WARNING: only safe on ascii
showBS :: (Show a) => a -> ByteString
showBS = fromString . show

-- | render a 'Bool' as @"true"@ or @"false"@
boolBS :: Bool -> ByteString
boolBS True  = "true"
boolBS False = "false"

-- | convert a 'Maybe' value to a 'Maybe' key/value pair.
mbParam :: ByteString         -- ^ param name
        -> Maybe a            -- ^ param value
        -> (a -> ByteString)  -- ^ function to convert 'a' to a 'ByteString'
        -> Maybe (ByteString, ByteString) 
mbParam _ Nothing _ = Nothing
mbParam name (Just v) show' = Just (name, show' v)

-- | HTTP methods used by Stripe requests
data SMethod
    = SGet
    | SPost [(ByteString, ByteString)]
    | SDelete
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''SMethod)

-- | a generalized type for representing Stripe API requests
--
-- 'ret' is a phantom type identifying what type of JSON Object is
-- expected in response to the request.
data StripeReq ret = StripeReq
    { srUrl         :: String                       -- ^ request URI
    , srQueryString :: [(ByteString, ByteString)]   -- ^ query string parameters
    , srMethod      :: SMethod                      -- ^ request method (includes form-data for 'SPost' method)
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeReq)

-- | 3-letter currency code. At present only @"usd"@ is supported.
-- see also: 'usd'
type Currency = Text

-- | 'Currency' code for @"usd"@.
usd :: Currency
usd = "usd"

-- | Timestamp in UTC Seconds
type Timestamp = Integer

-- | number of entries to return
type Count  = Integer

-- | offset from beginning of entry list
type Offset = Integer

-- | a list type that includes a count of the number of elements
-- FIXME: should this also have an offset? or is it even correct to have count?
data List a = List
    { count :: Integer
    , data_ :: [a]
    }

instance (FromJSON a) => FromJSON (List a) where
    parseJSON (Object obj) =
        List <$> obj .: "count"
             <*> obj .: "data"
    parseJSON _ = mzero

-- | unique key used to access the Stripe API
newtype ApiKey = ApiKey { unApiKey :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

------------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------------

-- | error that results when the user enters a card that can't be charged for some reason
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

-- | different types of errors that a Stripe request can generate
data ErrorType
    = CardError           -- ^ user entered a card that can't be charged for some reason
    | InvalidRequestError -- ^ request has invalid parameters
    | ApiError            -- ^ all other errors
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ErrorType)

instance FromJSON ErrorType where
    parseJSON (String str)
        | str == "card_error" = return CardError
        | str == "invalid_request_error" = return InvalidRequestError
        | str == "api_error"  = return ApiError
    parseJSON _ = mzero

-- | error that is returned when a @Stripe Request@ can not be processed.
data StripeError = StripeError
    { errorType :: ErrorType       -- ^ the type of error
    , message   :: Text            -- ^ A developer-facing message describing the error.
    , code      :: Maybe CardError -- ^ For card errors, additional information about the user-friendly message to display for this error (e.g. @"Your card was declined."@)
    , param     :: Maybe Text      -- ^ The parameter the error relates to if the error is parameter-specific. You can use this to display a message near the correct form field, for example.
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

-- | value returned by CVC, Address, and other checks
data Check
    = Pass         -- ^ value checked is correct
    | Fail         -- ^ value checked is incorrect
    | Unchecked    -- ^ customer's bank did not check the value
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

-- | describes the card used to make a 'Charge'
data Card = Card
    { cardExpMonth       :: Int
    , cardExpYear        :: Int
    , cardFingerprint    :: Text
    , cardLast4          :: Text
    , cardType           :: Text
    , cardAddrCity       :: Maybe Text  -- ^ Address City
    , cardAddrCountry    :: Maybe Text  -- ^ Billing address country, if provided when creating card
    , cardAddrLine1      :: Maybe Text  -- ^ Address Line 1
    , cardAddrLine1Check :: Maybe Check -- ^ If address line 1 was provided, results of the check
    , cardAddrLine2      :: Maybe Text  -- ^ Address Line 2
    , cardAddrState      :: Maybe Text  -- ^ Address State
    , cardAddrZip        :: Maybe Text  -- ^ Address Zipcode
    , cardAddrZipCheck   :: Maybe Check -- ^ If zipcode was provided, results of the check
    , cardCountry        :: Maybe Text  -- ^ Two-letter ISO code representing the country of the card (as accurately as we can determine it). You could use this attribute to get a sense of the international breakdown of cards you've collected.
    , cardCvcCheck       :: Maybe Check -- ^ If a CVC was provided, results of the check
    , cardName           :: Maybe Text  -- ^ Cardholder name
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

-- | cents
type Cents = Integer

-- | a unique id for a 'Customer'
--
-- see also: 'Customer', 'createCustomer'
newtype CustomerId = CustomerId { unCustomerId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)
