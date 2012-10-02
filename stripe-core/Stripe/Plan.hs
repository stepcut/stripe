{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
{- |

A subscription plan contains the pricing information for different
products and feature levels on your site. For example, you might have
a $10/month plan for basic features and a different $20/month plan for
premium features.

-}
module Stripe.Plan where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core

------------------------------------------------------------------------------
-- Plan
------------------------------------------------------------------------------


-- | an unique id which identifies a 'Plan'
newtype PlanId = PlanId { unPlanId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

-- | Frequency that a subscription should be billed
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


-- | A subscription plain
data Plan = Plan
    { planId              :: PlanId        -- ^ unique id
    , planLivemode        :: Bool
    , planAmount          :: Cents         -- ^ The amount in cents to be charged on the interval specified
    , planCurrency        :: Currency      -- ^ Currency in which subscription will be charged
    , planInterval        :: Interval      -- ^ The frequency with which a subscription should be billed.
    , planIntervalCount   :: Integer       -- ^ The number of the unit specified in the interval parameter. For example, you could specify an interval_count of 3 and an interval of 'month' for quarterly billing (every 3 months).
    , planName            :: Text          -- ^ Display name of the plan
    , planTrialPeriodDays :: Maybe Integer -- ^ Number of trial period days granted when subscribing a customer to this plan. Null if the plan has no trial period.
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

-- | You can create plans easily via the plan management page of the Stripe dashboard. Plan creation is also accessible via the API if you need to create plans on the fly.
createPlan :: PlanId        -- ^ Unique string of your choice that will be used to identify this plan when subscribing a customer. This could be an identifier like "gold" or a primary key from your own database.
           -> Cents         -- ^ A positive integer in cents (or 0 for a free plan) representing how much to charge (on a recurring basis)
           -> Currency      -- ^ 3-letter ISO code for currency. Currently, only 'usd' is supported.
           -> Interval      -- ^ Specifies billing frequency.
           -> Maybe Integer -- ^ The number of the unit specified in the interval parameter. For example, you could specify an interval_count of 3 and an interval of 'month' for quarterly billing (every 3 months).
           -> Text          -- ^ Name of the plan, to be displayed on invoices and in the web interface.
           -> Maybe Integer -- ^ Specifies a trial period in (an integer number of) days. If you include a trial period, the customer won't be billed for the first time until the trial period ends. If the customer cancels before the trial period is over, she'll never be billed at all.
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

-- | Retrieves the plan with the given ID.
getPlan :: PlanId -- ^ 'PlanId'
        -> StripeReq Plan
getPlan pid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SGet
              }

-- | Updates the name of a plan. Other plan details (price, interval, etc.) are, by design, not editable.
updatePlan :: PlanId  -- ^ 'PlanId'
           -> Text    -- ^ Name of the plan, to be displayed on invoices and in the web interface.
           -> StripeReq Plan
updatePlan pid name =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SPost [("name", Text.encodeUtf8 name)]
              }

-- | You can delete plans via the plan management page of the Stripe
-- dashboard. However, deleting a plan does not affect any current
-- subscribers to the plan; it merely means that new subscribers can't
-- be added to that plan. You can also delete plans via the API.
deletePlan :: PlanId
           -> StripeReq Plan
deletePlan pid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans/" ++ (Text.unpack $ unPlanId pid)
              , srQueryString = []
              , srMethod      = SDelete
              }

-- | Returns a list of your plans.
getPlans :: Maybe Count  -- ^ A limit on the number of plans to be returned. Count can range between 1 and 100 items.
         -> Maybe Offset -- ^ An offset into your plans array. The API will return the requested number of plans starting at that offset.
         -> StripeReq (List Plan)
getPlans mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/plans"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         ]

