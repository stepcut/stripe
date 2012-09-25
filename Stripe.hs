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
    , feeDetailCurrency    :: Currency
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

------------------------------------------------------------------------------
-- Coupon
------------------------------------------------------------------------------

newtype CouponId = CouponId { unCouponId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

------------------------------------------------------------------------------
-- Plan
------------------------------------------------------------------------------

newtype PlanId = PlanId { unPlanId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

data Interval
    = Month
    | Year
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Interval)

instance FromJSON Interval where
    parseJSON (String str)
        | str == "month"     = return Month
        | str == "year"      = return Year
    parseJSON _ = mzero

data Plan = Plan
    { planId              :: PlanId
    , planLivemode        :: Bool
    , planAmount          :: Cents
    , planCurrency        :: Currency
    , planInterval        :: Interval
    , planIntervalCount   :: Integer
    , planName            :: Text
    , planTrialPeriodDays :: Maybe Integer
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Plan)

instance FromJSON Plan where
    parseJSON (Object obj) =
        Plan <$> obj .: "id"
             <*> obj .: "livemode"
             <*> obj .: "amount"
             <*> obj .: "currency"
             <*> obj .: "interval"
             <*> obj .: "interval_count"
             <*> obj .: "name"
             <*> obj .: "trial_period_days"
    parseJSON _ = mzero

createPlan :: PlanId
           -> Cents
           -> Currency
           -> Interval
           -> Maybe Integer
           -> Text
           -> Maybe Integer
           -> StripeReq Plan
createPlan pid amount currency interval mIntervalCount name mTrialPeriodDays =
        StripeReq { srUrl         = "https://api.stripe.com/v1/plans"
                  , srQueryString = []
                  , srMethod      = SPost params
                  }
    where
      params = catMaybes
                 [ Just   ("amount", showBS amount)
                 , Just   ("currency", Text.encodeUtf8 currency)
                 , Just   ("interval", Text.encodeUtf8 (case interval of Month -> "month" ; Year -> "year"))
                 , mbParam "interval_count" mIntervalCount showBS
                 , Just   ("name", Text.encodeUtf8 name)
                 , mbParam "trial_period_days" mTrialPeriodDays showBS
                 ]

getPlan :: PlanId
        -> StripeReq Plan
getPlan pid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SGet
              }

updatePlan :: PlanId
            -> Text
            -> StripeReq Plan
updatePlan pid name =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SPost [("name", Text.encodeUtf8 name)]
              }

deletePlan :: PlanId
           -> StripeReq Plan
deletePlan pid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SDelete
              }

data Plans = Plans
    { planCount :: Integer
    , planData  :: [Plan]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Plans)

instance FromJSON Plans where
    parseJSON (Object obj) =
        Plans <$> obj .: "count"
              <*> obj .: "data"
    parseJSON _ = mzero

getPlans :: Maybe Count
         -> Maybe Offset
         -> StripeReq Plans
getPlans mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         ]

------------------------------------------------------------------------------
-- CardInfo
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- CardToken
------------------------------------------------------------------------------


newtype CardTokenId = CardTokenId { unCardTokenId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CardToken = CardToken
    { cardTokenId       :: CardTokenId
    , cardTokenLivemode :: Bool
    , cardTokenCard     :: Card
    , cardTokenCreated  :: Timestamp
    , cardTokenUsed     :: Bool
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardToken)

createCardToken :: CardInfo
                -> StripeReq CardToken
createCardToken cardInfo =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens"
              , srQueryString = []
              , srMethod      = SPost (cardInfoPairs cardInfo)
              }

getCardToken :: CardTokenId
             -> StripeReq CardToken
getCardToken cti =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens/" ++ Text.unpack (unCardTokenId cti)
              , srQueryString = []
              , srMethod      = SGet
              }

------------------------------------------------------------------------------
-- Charge
------------------------------------------------------------------------------

newtype ChargeId = ChargeId { unChargeId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)
             
data Charge = Charge
    { chargeId             :: ChargeId
    , chargeLivemode       :: Bool
    , chargeAmount         :: Cents
    , chargeCard           :: Card
    , chargeTimestamp      :: Timestamp
    , chargeCurrency       :: Currency
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

getCharges :: Maybe Count
           -> Maybe Offset
           -> Maybe CustomerId
           -> StripeReq Charges
getCharges mCount mOffset mCustomerId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         , mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                         ]

data ChargeTo
    = CI CardInfo
    | CT CardTokenId
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
              , srMethod      = SPost params
              }
    where
      params =
               [ ("amount", showBS amount)
               , ("currency", Text.encodeUtf8 currency)
               ] ++ 
               (case chargeTo of
                   (CS (CustomerId  ci)) ->
                       [ ("customer", Text.encodeUtf8 ci)]
                   (CT (CardTokenId ct)) ->
                       [ ("card", Text.encodeUtf8 ct)]
                   (CI ci) -> cardInfoPairs ci
               )

retrieveCharge :: ChargeId
               -> StripeReq Charge
retrieveCharge cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid)
              , srQueryString = []
              , srMethod      = SGet
              }


refundCharge :: ChargeId
             -> Maybe Cents
             -> StripeReq Charge
refundCharge cid mCents =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid) ++ "/refund"
              , srQueryString = []
              , srMethod      = SPost $ case mCents of
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

createCustomer :: Maybe (Either CardTokenId CardInfo)
               -> Maybe CouponId
               -> Maybe Text -- ^ email
               -> Maybe Text -- ^ description
               -> Maybe Integer -- ^ acount balance
               -> Maybe PlanId
               -> Maybe Timestamp -- ^ trial end
               -> Maybe Integer -- ^ quantity
               -> StripeReq Customer
createCustomer mCard mCouponId mEmail mDescription mBalance mPlanId mTimestamp mQuantity =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers"
              , srQueryString = []
              , srMethod      =
                  SPost  $
                        cardParams mCard ++
                        (catMaybes
                      [ mbParam "coupon"          mCouponId    (showBS . unCouponId)
                      , mbParam "email"           mEmail       Text.encodeUtf8
                      , mbParam "description"     mDescription Text.encodeUtf8
                      , mbParam "account_balance" mBalance     showBS
                      , mbParam "plan"            mPlanId      (showBS . unPlanId)
                      , mbParam "trial_end"       mTimestamp   showBS
                      , mbParam "quantity"        mQuantity    showBS
                      ])
              }
    where
      cardParams (Just (Left (CardTokenId ct))) =
          [("card", Text.encodeUtf8 ct)]
      cardParams (Just (Right ci)) =
          cardInfoPairs ci
      cardParams Nothing =
          []

updateCustomer :: CustomerId
               -> Maybe (Either CardTokenId CardInfo)
               -> Maybe CouponId
               -> Maybe Text -- ^ email
               -> Maybe Text -- ^ description
               -> Maybe Integer -- ^ acount balance
               -> StripeReq Customer
updateCustomer cid mCard mCouponId mEmail mDescription mBalance =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId cid)
              , srQueryString = []
              , srMethod      =
                  SPost  $
                        cardParams mCard ++
                        (catMaybes
                      [ mbParam "coupon"          mCouponId    (showBS . unCouponId)
                      , mbParam "email"           mEmail       Text.encodeUtf8
                      , mbParam "description"     mDescription Text.encodeUtf8
                      , mbParam "account_balance" mBalance     showBS
                      ])
              }
    where
      cardParams (Just (Left (CardTokenId ct))) =
          [("card", Text.encodeUtf8 ct)]
      cardParams (Just (Right ci)) =
          cardInfoPairs ci
      cardParams Nothing =
          []

getCustomer :: CustomerId
            -> StripeReq Customer
getCustomer cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ (Text.unpack $ unCustomerId cid)
              , srQueryString = []
              , srMethod      = SGet
              }

data Customers = Customers
    { customersCount :: Integer
    , customersData  :: [Customer]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Customers)

instance FromJSON Customers where
    parseJSON (Object obj) =
        Customers <$> obj .: "count"
                  <*> obj .: "data"
    parseJSON _ = mzero


getCustomers :: Maybe Count
             -> Maybe Offset
             -> StripeReq Customers
getCustomers mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         ]


deleteCustomer :: CustomerId
               -> StripeReq Customer
deleteCustomer cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ (Text.unpack $ unCustomerId cid)
              , srQueryString = []
              , srMethod      = SDelete
              }

------------------------------------------------------------------------------
-- Discount
------------------------------------------------------------------------------

data Discount = Discount

------------------------------------------------------------------------------
-- Subscription
------------------------------------------------------------------------------

data Subscription = Subscription


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
    do let req' = (fromJust $ parseUrl srUrl) { queryString = W.renderSimpleQuery False srQueryString 
                                              } 
           req = case srMethod of
                   SGet -> req'
                   (SPost params) -> urlEncodedBody params req'
                   SDelete        -> req' { method = "DELETE" }
       res <- httpLbs (applyBasicAuth k "" (req { checkStatus = \_ _ -> Nothing})) manager
       liftIO $ print $ responseStatus res
       liftIO $ putStrLn $ Text.unpack $ Text.decodeUtf8 $ toStrict $ responseBody  res
       if W.statusCode (responseStatus res) == 200
          then return $ Right $ fromJust $ decode' (responseBody res)
          else return $ Left  $ fromJust $ decode' (responseBody res)
