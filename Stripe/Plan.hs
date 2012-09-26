{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
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
