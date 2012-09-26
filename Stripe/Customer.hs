{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe.Customer where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Stripe.Core         
import Stripe.Discount     (Discount)

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
    , customerDiscount :: Maybe Discount
    , customerEmail :: Maybe Text
    , customerSubscription :: Maybe Subscription
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
                 <*> obj .: "discount"
                 <*> obj .: "email"
                 <*> obj .: "subscription"
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
