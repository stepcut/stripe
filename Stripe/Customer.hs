{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
{- |

'Customer' objects allow you to perform recurring charges and track
multiple charges that are associated with the same customer. The API
allows you to create, delete, and update your customers. You can
retrieve individual customers as well as a list of all your customers.

-}
module Stripe.Customer where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text as Text (Text,unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core
import Stripe.Coupon       (CouponId(..))
import Stripe.Discount     (Discount)
import Stripe.Plan         (PlanId(..))
import Stripe.Subscription (Subscription)
import Stripe.Token        (CardInfo(..), CardTokenId(..), cardInfoPairs)

------------------------------------------------------------------------------
-- Customer
------------------------------------------------------------------------------

-- | a 'Customer'
data Customer = Customer
    { customerId             :: CustomerId
    , customerLivemode       :: Bool
    , customerCreated        :: Timestamp
    , customerAccountBalance :: Maybe Integer      -- ^ Current balance, if any, being stored on the customer's account. If negative, the customer has credit to apply to the next invoice. If positive, the customer has an amount owed that will be added to the next invoice. The balance does not refer to any unpaid invoices; it solely takes into account amounts that have yet to be successfully applied to any invoice. This balance is only taken into account for recurring charges.
    , customerActiveCard     :: Maybe Card         -- ^ Customer 'Card'
    , customerDeliquent      :: Maybe Bool         -- ^ Whether or not the latest charge for the customer's latest invoice has failed
    , customerDescription    :: Maybe Text         -- ^ description
    , customerDiscount       :: Maybe Discount     -- ^ customer 'Discount'
    , customerEmail          :: Maybe Text         -- ^ customer email
    , customerSubscription   :: Maybe Subscription -- ^ the current 'Subscription' on the customer
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

-- | Creates a new 'Customer'
createCustomer :: Maybe (Either CardTokenId CardInfo)  -- ^ A card to attach to the customer
               -> Maybe CouponId  -- ^ If you provide a coupon code, the customer will have a discount applied on all recurring charges. Charges you create through the API will not have the discount. You can manage your coupons in the coupon section of your account.
               -> Maybe Text      -- ^ The customer's email address. It is displayed alongside the customer in the web interface and can be useful for searching and tracking.
               -> Maybe Text      -- ^ An arbitrary string which you can attach to a customer object. It is displayed alongside the customer in the web interface.
               -> Maybe Cents     -- ^ An integer amount in cents that is the starting account balance for your customer. A negative amount represents a credit that will be used before attempting any charges to the customer's card; a positive amount will be added to the next invoice.
               -> Maybe PlanId    -- ^ The identifier of the 'Plan' to subscribe the 'Customer' to. If provided, the returned 'Customer' object has a 'customerSubscription' attribute describing the state of the customer's 'Subscription'.
               -> Maybe Timestamp -- ^ UTC integer timestamp representing the end of the trial period the customer will get before being charged for the first time. If set, trial_end will override the default trial period of the plan the customer is being subscribed to.
               -> Maybe Integer   -- ^ The quantity you'd like to apply to the subscription you're creating. For example, if your plan is $10/\user\/month, and your customer has 5 users, you could pass 5 as the quantity to have the customer charged $50 (5 x $10) monthly.
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

-- | Updates the specified 'Customer' by setting the values of the
-- parameters passed. Any parameters not provided will be left
-- unchanged. For example, if you pass the 'Card' parameter, that
-- becomes the customer's active card to be used for all charges in
-- future. When you update a customer to a new valid card, the last
-- unpaid 'Invoice' (if one exists) will be retried automatically.
--
-- This request accepts mostly the same arguments as the 'createCustomer'
-- call. However, 'Subscription'-related arguments ('Plan' and
-- 'trialEnd') are not accepted. To change those, one must update the
-- customer's subscription directly.
updateCustomer :: CustomerId
               -> Maybe (Either CardTokenId CardInfo) -- ^ A new card to attach to the customer.
               -> Maybe CouponId -- ^ If you provide a coupon code, the customer will have a discount applied on all recurring charges. Charges you create through the API will not have the discount. You can manage your coupons in the coupon section of your account.
               -> Maybe Text     -- ^ The customer's email address. It is displayed alongside the customer in the web interface and can be useful for searching and tracking.
               -> Maybe Text     -- ^ An integer amount in cents that represents account credit or debit. A negative amount represents a credit that will be used before attempting any charges to the customer's card; a positive amount will be added to the next invoice. You might update this property if the customer has credit that you want to reset to 0, for example.
               -> Maybe Cents    -- ^ An integer amount in cents that represents account credit or debit. A negative amount represents a credit that will be used before attempting any charges to the customer's card; a positive amount will be added to the next invoice. You might update this property if the customer has credit that you want to reset to 0, for example.
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

-- | Retrieves the details of an existing customer.
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

-- | Returns a list of your customers. The customers are returned sorted by creation date, with the most recently created customers appearing first.
getCustomers :: Maybe Count  -- ^ A limit on the number of customers to be returned. Count can range between 1 and 100 customers. /default 10/
             -> Maybe Offset -- ^ An offset into your customer array. The API will return the requested number of customers starting at that offset. /default 0/
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

-- | Permanently deletes a customer. It cannot be undone.
deleteCustomer :: CustomerId
               -> StripeReq Customer
deleteCustomer cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ (Text.unpack $ unCustomerId cid)
              , srQueryString = []
              , srMethod      = SDelete
              }
