{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Stripe where

import Control.Applicative ((<$>), (<*>))
import Control.Monad        (mzero)
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Conduit
import Data.Data     (Data, Typeable)
import Data.Maybe
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.String   (fromString)
import Data.Text     (Text)
import qualified Data.Text.Encoding as Text
import Control.Monad.Trans.Control
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W

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
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

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

type Cents = Integer

data FeeDetail = FeeDetail
    { feeDetailAmount      :: Cents
    , feeDetailCurrency    :: Text
    , feeDetailType        :: Text
    , feeDetailApplication :: Text
    , feeDetailDescription :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeeDetail)

instance FromJSON FeeDetail where
    parseJSON (Object obj) =
        FeeDetail <$> obj .: "amount"
                  <*> obj .: "currency"
                  <*> obj .: "type"
                  <*> obj .: "application"
                  <*> obj .: "description"
    parseJSON _ = mzero

type Timestamp = Integer    
             
data Charge = Charge
    { chargeId             :: Text
    , chargeLivemode       :: Bool
    , chargeAmount         :: Cents
    , chargeCard           :: Card
    , chargeTimestamp      :: Timestamp
    , chargeCurrency       :: Text
    , chargeDisputed       :: Bool
    , chargeFee            :: Cents
    , chargeFeeDetails     :: [FeeDetail]
    , chargePaid           :: Bool
    , chargeRefunded       :: Bool -- false for partial refund
    , chargeAmountRefunded :: Cents
    , chargeCustomer       :: CustomerId
    , chargeDescription    :: Text
    , chargeFailureMessage :: Text
    , chargeInvoice              :: Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Charge)

instance FromJSON Charge where
    parseJSON (Object obj) =
        Charge <$> obj .: "id"
               <*> obj .: "livemode"
               <*> obj .: "amount"
               <*> obj .: "card"
               <*> obj .: "created"
               <*> obj .: "currency"
               <*> obj .: "disputed"
               <*> obj .: "fee"
               <*> obj .: "fee_details"
               <*> obj .: "paid"
               <*> obj .: "refunded"
               <*> obj .: "amount_refunded"
               <*> obj .: "customer"
               <*> obj .: "description"
               <*> obj .: "failure_message"
               <*> obj .: "invoice"
    parseJSON _ = mzero

data Charges = Charges
    { chargesCount :: Integer
    , chargesData  :: [Charge]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Charges)

instance FromJSON Charges where
    parseJSON (Object obj) =
        Charges <$> obj .: "count"
                <*> obj .: "data"
    parseJSON _ = mzero

type Count = Integer
type Offset = Integer

newtype StripeReq m ret = StripeReq { unSR :: Request m }

charges :: Maybe Count -> Maybe Offset -> Maybe CustomerId -> StripeReq m Charges
charges mCount mOffset mCustomerId =
    let (Just req) = parseUrl "https://api.stripe.com/v1/charges"
    in StripeReq $ req { queryString = W.renderSimpleQuery False params  }
    where
      params = catMaybes [ mbParam "count" mCount (fromString . show)
                         , mbParam "offset" mOffset (fromString . show)
                         , mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                         ]
      mbParam :: ByteString -> Maybe a -> (a -> ByteString) -> Maybe W.SimpleQueryItem
      mbParam _ Nothing _ = Nothing
      mbParam name (Just v) show' = Just (name, show' v)


stripe :: ( MonadResource m
          , MonadBaseControl IO m
          , FromJSON a
          ) => ApiKey -> StripeReq m a -> Manager -> m (Maybe a)
stripe (ApiKey k) (StripeReq req) manager =
    do res <- httpLbs (applyBasicAuth k "" req) manager
       return $ decode' (responseBody res)
