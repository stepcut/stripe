{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe.Subscription where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core         
import Stripe.Coupon       (CouponId(..), Coupon)
import Stripe.Plan         (PlanId(..), Plan(..))
import Stripe.Token        (CardInfo(..), CardTokenId(..), cardInfoPairs)

------------------------------------------------------------------------------
-- Subscription
------------------------------------------------------------------------------

data SubscriptionStatus
    = Trialing
    | Active
    | PastDue
    | Canceled
    | Unpaid
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''SubscriptionStatus)

instance FromJSON SubscriptionStatus where
    parseJSON (String str)
        | str == "trialing" = return Trialing
        | str == "active"   = return Active
        | str == "past_due" = return PastDue
        | str == "canceled" = return Canceled
        | str == "unpaid"   = return Unpaid
    parseJSON _ = mzero

data Subscription = Subscription
    { subCancelAtPeriodEnd  :: Bool
    , subCustomerId         :: CustomerId
    , subPlan               :: Plan
    , subQuantity           :: Integer
    , subStart              :: Timestamp
    , subStatus             :: SubscriptionStatus
    , subCancelledAt        :: Maybe Timestamp
    , subCurrentPeriodStart :: Maybe Timestamp
    , subCurrentPeriodEnd   :: Maybe Timestamp
    , subEndedAt            :: Maybe Timestamp
    , subTrialStart         :: Maybe Timestamp
    , subTrialEnd           :: Maybe Timestamp
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Subscription)

instance FromJSON Subscription where
    parseJSON (Object obj) =
        Subscription <$> obj .: "cancel_at_period_end"
                     <*> obj .: "customer"
                     <*> obj .: "plan"
                     <*> obj .: "quantity"
                     <*> obj .: "start"
                     <*> obj .: "status"
                     <*> obj .: "canceled_at"
                     <*> obj .: "current_period_start"
                     <*> obj .: "current_period_end"
                     <*> obj .: "ended_at"
                     <*> obj .: "trial_start"
                     <*> obj .: "trial_end"
    parseJSON _ = mzero

updateSubscription :: CustomerId
                   -> PlanId
                   -> Maybe CouponId
                   -> Maybe Bool
                   -> Maybe Timestamp
                   -> Maybe (Either CardTokenId CardInfo)
                   -> Maybe Integer
                   -> StripeReq Subscription
updateSubscription cid planId mCouponId mProrate mTrialEnd mCard mQuantity =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId cid) ++ "/subscription"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = (catMaybes [ Just ("plan", Text.encodeUtf8 (unPlanId planId))
                          , mbParam "coupon"    mCouponId (Text.encodeUtf8 . unCouponId)
                          , mbParam "prorate"   mProrate  boolBS
                          , mbParam "trial_end" mTrialEnd showBS
                          , mbParam "quantity"  mQuantity showBS
                          ]) ++ case mCard of
                                  Nothing -> []
                                  (Just (Left (CardTokenId cti))) -> [("card", Text.encodeUtf8 cti)]
                                  (Just (Right cardInfo)) -> cardInfoPairs cardInfo

cancelSubscription :: CustomerId
                   -> Maybe Bool
                   -> StripeReq Subscription
cancelSubscription cid mAtPeriodEnd =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId cid) ++ "/subscription"
              , srQueryString = maybe [] (\b -> [("at_period_end", boolBS b)]) mAtPeriodEnd
              , srMethod      = SDelete
              }

