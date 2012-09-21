{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Stripe where

import Control.Applicative ((<$>), (<*>))
import Control.Monad        (mzero)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Conduit
import Data.Data     (Data, Typeable)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe
import Data.Monoid
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.String   (fromString)
import Data.Text     as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Control.Monad.Trans.Control
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W

toStrict :: BL.ByteString -> ByteString
toStrict = mconcat . BL.toChunks

-- WARNING: only safe on ascii
showBS :: (Show a) => a -> ByteString
showBS = fromString . show

mbParam :: ByteString -> Maybe a -> (a -> ByteString) -> Maybe (ByteString, ByteString)
mbParam _ Nothing _ = Nothing
mbParam name (Just v) show' = Just (name, show' v)


{-
mbItem :: ByteString -> (a -> ByteString) -> Maybe a -> Maybe (ByteString, ByteString)
mbItem k f Nothing = Nothing
mbItem k f (Just v) = Just (k, f v)
-}

data StripeReq ret = StripeReq
    { srUrl         :: String
    , srQueryString :: [(ByteString, ByteString)]
    , srPostData    :: Maybe [(ByteString, ByteString)]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeReq)

usd :: Text
usd = "usd"

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

data FeeDetail = FeeDetail
    { feeDetailAmount      :: Cents
    , feeDetailCurrency    :: Text
    , feeDetailType        :: Text
    , feeDetailApplication :: Maybe Text
    , feeDetailDescription :: Maybe Text
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

newtype ChargeId = ChargeId { unChargeId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)
             
data Charge = Charge
    { chargeId             :: ChargeId
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
    , chargeAmountRefunded :: Maybe Cents
    , chargeCustomer       :: Maybe CustomerId
    , chargeDescription    :: Maybe Text
    , chargeFailureMessage :: Maybe Text
    , chargeInvoice        :: Maybe Text
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

type Count  = Integer
type Offset = Integer

charges :: Maybe Count
        -> Maybe Offset
        -> Maybe CustomerId
        -> StripeReq Charges
charges mCount mOffset mCustomerId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges"
              , srQueryString = params
              , srPostData    = Nothing
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         , mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                         ]

type Currency = Text

data CardInfo = CardInfo
    { cardInfoNumber      :: Text
    , cardInfoExpMonth    :: Int
    , cardInfoExpYear     :: Int
    , cardInfoCvc         :: Maybe Int
    , cardInfoName        :: Maybe Text
    , cardInfoAddr1       :: Maybe Text
    , cardInfoAddr2       :: Maybe Text
    , cardInfoAddrZip     :: Maybe Text
    , cardInfoAddrState   :: Maybe Text
    , cardInfoAddrCountry :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardInfo)

newtype CardToken = CardToken { unCardToken :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data ChargeTo
    = CI CardInfo
    | CT CardToken
    | CS CustomerId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ChargeTo)

createCharge :: Cents  -- ^ amount
             -> Currency 
             -> ChargeTo
             -> Maybe Text -- ^ description
             -> StripeReq Charge
createCharge amount currency chargeTo description =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges"
              , srQueryString = []
              , srPostData    = Just params
              }
    where
      params =
               [ ("amount", showBS amount)
               , ("currency", Text.encodeUtf8 currency)
               ] ++ 
               (case chargeTo of
                   (CS (CustomerId  ci)) ->
                       [ ("customer", Text.encodeUtf8 ci)]
                   (CT (CardToken ct)) ->
                       [ ("card", Text.encodeUtf8 ct)]
                   (CI ci) -> cardInfoPairs ci
               )

cardInfoPairs :: CardInfo -> [(ByteString, ByteString)]
cardInfoPairs (CardInfo{..}) =
    catMaybes [ Just  ("card[number]", Text.encodeUtf8 cardInfoNumber)
              , Just  ("card[exp_month]", showBS cardInfoExpMonth)
              , Just  ("card[exp_year]", showBS cardInfoExpYear)
              , mbParam "card[cvc]"             cardInfoCvc         showBS 
              , mbParam "card[name]"            cardInfoName        Text.encodeUtf8 
              , mbParam "card[address_line1]"   cardInfoAddr1       Text.encodeUtf8 
              , mbParam "card[address_line2]"   cardInfoAddr2       Text.encodeUtf8 
              , mbParam "card[address_zip]"     cardInfoAddrZip     Text.encodeUtf8 
              , mbParam "card[address_state]"   cardInfoAddrState   Text.encodeUtf8 
              , mbParam "card[address_country]" cardInfoAddrCountry Text.encodeUtf8 
              ]


retrieveCharge :: ChargeId
               -> StripeReq Charge
retrieveCharge cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid)
              , srQueryString = []
              , srPostData    = Nothing
              }


refundCharge :: ChargeId
             -> Maybe Cents
             -> StripeReq Charge
refundCharge cid mCents =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid) ++ "/refund"
              , srQueryString = []
              , srPostData    = Just $ case mCents of
                                         Nothing      -> []
                                         (Just cents) -> [("amount", showBS cents)]
              }

------------------------------------------------------------------------------
-- Customer
------------------------------------------------------------------------------

data Customer = Customer
    { customerId :: CustomerId
    , customerLivemode :: Bool
    , customerCreated  :: Timestamp
    , customerAccountBalance :: Maybe Integer
    , customerActiveCard :: Card
    , customerDeliquent :: Maybe Bool
    , customerDescription :: Maybe Text
--    , customerDiscount :: Maybe Discount
    , customerEmail :: Maybe Text
--    , customerSubscription :: Maybe Subscription
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Customer)

instance FromJSON Customer where
    parseJSON (Object obj) =
        Customer <$> obj .: "id"
                 <*> obj .: "livemode"
                 <*> obj .: "created"
                 <*> obj .: "account_balance"
                 <*> obj .: "active_card"
                 <*> obj .: "delinquent"
                 <*> obj .: "description"
--                 <*> obj .: "discount"
                 <*> obj .: "email"
--                 <*> obj .: "subscription"
    parseJSON _ = mzero

createCustomer :: Either CardInfo CardToken 
               -> Maybe Coupon
               -> Maybe Text -- ^ email
               -> Maybe Text -- ^ description
               -> Maybe Integer -- ^ acount balance
               -> Maybe Plan
               -> Maybe Timestamp -- ^ trial end
               -> Maybe Integer -- ^ quantity
               -> StripeReq Customer
createCustomer = undefined

------------------------------------------------------------------------------
-- Discount
------------------------------------------------------------------------------

data Discount = Discount

------------------------------------------------------------------------------
-- Subscription
------------------------------------------------------------------------------

data Subscription = Subscription

------------------------------------------------------------------------------
-- Coupon
------------------------------------------------------------------------------

data Coupon = Coupon

------------------------------------------------------------------------------
-- Plan
------------------------------------------------------------------------------

data Plan = Plan


------------------------------------------------------------------------------
-- 
------------------------------------------------------------------------------

stripe :: ( MonadResource m
          , MonadBaseControl IO m
          , FromJSON a
          ) => ApiKey
       -> StripeReq a
       -> Manager
       -> m (Either StripeError a)
stripe (ApiKey k) (StripeReq{..}) manager =
    do let req = maybe id urlEncodedBody srPostData $
                  (fromJust $ parseUrl srUrl)
                    { queryString = W.renderSimpleQuery False srQueryString 
                    } 
       res <- httpLbs (applyBasicAuth k "" (req { checkStatus = \_ _ -> Nothing})) manager
       liftIO $ print $ responseStatus res
       liftIO $ putStrLn $ Text.unpack $ Text.decodeUtf8 $ toStrict $ responseBody  res
       if W.statusCode (responseStatus res) == 200
          then return $ Right $ fromJust $ decode' (responseBody res)
          else return $ Left  $ fromJust $ decode' (responseBody res)
