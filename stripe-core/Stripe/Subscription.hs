{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
{- |

Subscriptions allow you to charge a customer's card on a recurring
basis. A 'Subscription' ties a customer to a particular 'Plan' you've
created.

-}
module Stripe.Subscription where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (Migrate(..), SafeCopy, base, deriveSafeCopy, extension)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core
import Stripe.Coupon       (CouponId(..), Coupon)
import Stripe.Plan         (PlanId(..), Plan(..))
import Stripe.Token        (CardInfo(..), CardTokenId(..), cardInfoPairs)

------------------------------------------------------------------------------
-- Subscription
------------------------------------------------------------------------------

-- | status of a 'Subscription'
data SubscriptionStatus
    = Trialing
    | Active
    | PastDue
    | Canceled
    | Unpaid
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''SubscriptionStatus)

newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

instance FromJSON SubscriptionStatus where
    parseJSON (String str)
        | str == "trialing" = return Trialing
        | str == "active"   = return Active
        | str == "past_due" = return PastDue
        | str == "canceled" = return Canceled
        | str == "unpaid"   = return Unpaid
    parseJSON _ = mzero

-- | A 'Subscription' to 'Plan'
data Subscription_0 = Subscription_0
    { subCancelAtPeriodEnd_0  :: Bool               -- ^ If the subscription has been canceled with the at_period_end flag set to true, cancel_at_period_end on the subscription will be true. You can use this attribute to determine whether a subscription that has a status of active is scheduled to be canceled at the end of the current period.
    , subCustomerId_0         :: CustomerId         -- ^ 'CustomerId'
    , subPlan_0               :: Plan               -- ^ 'Plan' the customer is subscribed to
    , subQuantity_0           :: Integer            -- ^ The quantity you'd like to apply to the subscription you're creating.
    , subStart_0              :: Timestamp          -- ^ Date the subscription started
    , subStatus_0             :: SubscriptionStatus -- ^ A subscription still in its trial period is 'Trialing' and moves to 'Active' when the trial period is over. When payment to renew the subscription fails, the subscription becomes 'PastDue.' After Stripe has exhausted all payment retry attempts, the subscription ends up with a status of either 'Canceled' or 'Unpaid' depending on your retry settings.
    , subCancelledAt_0        :: Maybe Timestamp    -- ^ If the subscription has been canceled, the date of that cancellation. If the subscription was canceled with 'subCancelAtPeriodEnd', 'subCanceledAt' will still reflect the date of the initial cancellation request, not the end of the subscription period when the subscription is automatically moved to a canceled state.
    , subCurrentPeriodEnd_0   :: Maybe Timestamp    -- ^ End of the current period that the subscription has been invoiced for. At the end of this period, a new invoice will be created.
    , subCurrentPeriodStart_0 :: Maybe Timestamp    -- ^ Start of the current period that the subscription has been invoiced for
    , subEndedAt_0            :: Maybe Timestamp    -- ^ If the subscription has ended (either because it was canceled or because the customer was switched to a subscription to a new plan), the date the subscription ended
    , subTrialEnd_0           :: Maybe Timestamp    -- ^ If the subscription has a trial, the end of that trial.
    , subTrialStart_0         :: Maybe Timestamp    -- ^ If the subscription has a trial, the beginning of that trial.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Subscription_0)

-- | A 'Subscription' to 'Plan'
data Subscription = Subscription
    { subSubscriptionId     :: SubscriptionId
    , subCancelAtPeriodEnd  :: Bool               -- ^ If the subscription has been canceled with the at_period_end flag set to true, cancel_at_period_end on the subscription will be true. You can use this attribute to determine whether a subscription that has a status of active is scheduled to be canceled at the end of the current period.
    , subCustomerId         :: CustomerId         -- ^ 'CustomerId'
    , subPlan               :: Plan               -- ^ 'Plan' the customer is subscribed to
    , subQuantity           :: Integer            -- ^ The quantity you'd like to apply to the subscription you're creating.
    , subStart              :: Timestamp          -- ^ Date the subscription started
    , subStatus             :: SubscriptionStatus -- ^ A subscription still in its trial period is 'Trialing' and moves to 'Active' when the trial period is over. When payment to renew the subscription fails, the subscription becomes 'PastDue.' After Stripe has exhausted all payment retry attempts, the subscription ends up with a status of either 'Canceled' or 'Unpaid' depending on your retry settings.
    , subAppFeePercent      :: Maybe Int          -- ^ A positive decimal that represents the fee percentage of the subscription invoice amount that will be transferred to the application owner’s Stripe account each billing period.
    , subCancelledAt        :: Maybe Timestamp    -- ^ If the subscription has been canceled, the date of that cancellation. If the subscription was canceled with 'subCancelAtPeriodEnd', 'subCanceledAt' will still reflect the date of the initial cancellation request, not the end of the subscription period when the subscription is automatically moved to a canceled state.
    , subCurrentPeriodEnd   :: Maybe Timestamp    -- ^ End of the current period that the subscription has been invoiced for. At the end of this period, a new invoice will be created.
    , subCurrentPeriodStart :: Maybe Timestamp    -- ^ Start of the current period that the subscription has been invoiced for
    , subEndedAt            :: Maybe Timestamp    -- ^ If the subscription has ended (either because it was canceled or because the customer was switched to a subscription to a new plan), the date the subscription ended
    , subTrialEnd           :: Maybe Timestamp    -- ^ If the subscription has a trial, the end of that trial.
    , subTrialStart         :: Maybe Timestamp    -- ^ If the subscription has a trial, the beginning of that trial.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 1 'extension ''Subscription)

instance Migrate Subscription where
     type MigrateFrom Subscription = Subscription_0
     migrate (Subscription_0{..}) =
         Subscription
         { subSubscriptionId     = SubscriptionId ""
         , subCancelAtPeriodEnd  = subCancelAtPeriodEnd_0
         , subCustomerId         = subCustomerId_0
         , subPlan               = subPlan_0
         , subQuantity           = subQuantity_0
         , subStart              = subStart_0
         , subStatus             = subStatus_0
         , subAppFeePercent      = Nothing
         , subCancelledAt        = subCancelledAt_0
         , subCurrentPeriodEnd   = subCurrentPeriodEnd_0
         , subCurrentPeriodStart = subCurrentPeriodStart_0
         , subEndedAt            = subEndedAt_0
         , subTrialEnd           = subTrialEnd_0
         , subTrialStart         = subTrialStart_0
         }

instance FromJSON Subscription where
    parseJSON (Object obj) =
        Subscription <$> obj .: "id"
                     <*> obj .: "cancel_at_period_end"
                     <*> obj .: "customer"
                     <*> obj .: "plan"
                     <*> obj .: "quantity"
                     <*> obj .: "start"
                     <*> obj .: "status"
                     <*> obj .: "application_fee_percent"
                     <*> obj .: "canceled_at"
                     <*> obj .: "current_period_end"
                     <*> obj .: "current_period_start"
                     <*> obj .: "ended_at"
                     <*> obj .: "trial_end"
                     <*> obj .: "trial_start"
    parseJSON _ = mzero


-- | Subscribes a customer to a plan, meaning the customer will be
-- billed monthly starting from signup. If the customer already has an
-- active subscription, we'll update it to the new plan and optionally
-- prorate the price we charge next month to make up for any price
-- changes.
--
-- By default, we prorate subscription changes. For example, if a
-- customer signs up on May 1 for a $10 plan, she'll be billed $10
-- immediately. If she then switches to a $20 plan on May 15, on June
-- 1 she'll be billed $25 ($20 for a renewal of her subscription and a
-- $5 prorating adjustment for the previous month). Similarly, a
-- downgrade will generate a credit to be applied to the next
-- invoice. We also prorate when you make quantity changes. Switching
-- plans does not change the billing date or generate an immediate
-- charge unless you're switching between different intervals
-- (e.g. monthly to yearly), in which case we apply a credit for the
-- time unused on the old plan and charge for the new plan starting
-- right away, resetting the billing date.
--
-- If you don't want to prorate, set the prorate option to false and
-- the customer would be billed $10 on May 1 and $20 on June
-- 1. Similarly, if you set prorate to false when switching between
-- different billing intervals (monthly to yearly, for example), we
-- won't generate any credits for the old subscription's unused time,
-- although we will still reset the billing date and bill immediately
-- for the new subscription.
updateSubscription :: CustomerId
                   -> PlanId          -- ^ The identifier of the plan to subscribe the customer to.
                   -> Maybe CouponId  -- ^ The code of the coupon to apply to the customer if you would like to apply it at the same time as creating the subscription.
                   -> Maybe Bool      -- ^ Flag telling us whether to prorate switching plans during a billing cycle
                   -> Maybe Timestamp -- ^ UTC integer timestamp representing the end of the trial period the customer will get before being charged for the first time. If set, 'subTrialEnd' will override the default trial period of the plan the customer is being subscribed to.
                   -> Maybe (Either CardTokenId CardInfo) -- ^ A new card to attach to the customer
                   -> Maybe Integer   -- ^ The quantity you'd like to apply to the subscription you're creating. For example, if your plan is $10\/user\/month, and your customer has 5 users, you could pass 5 as the quantity to have the customer charged $50 (5 x $10) monthly. If you update a subscription but don't change the plan ID (e.g. changing only the trial_end), the subscription will inherit the old subscription's quantity attribute unless you pass a new quantity parameter. If you update a subscription and change the plan ID, the new subscription will not inherit the quantity attribute and will default to 1 unless you pass a quantity parameter.
                   -> StripeReq Subscription
updateSubscription cid planId mCouponId mProrate mTrialEnd mCard mQuantity =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId cid) ++ "/subscription"
              , srQueryString = []
              , srMethod      = SPost params
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

-- | Cancels the subscription if it exists. If you set the
-- at_period_end parameter to true, the subscription will remain
-- active until the end of the period, at which point it will be
-- cancelled and not renewed. By default, the subscription is
-- terminated immediately. In either case, the customer will not be
-- charged again for the subscription. Note, however, that any pending
-- invoice items that you've created will still be charged for at the
-- end of the period unless manually deleted.If you've set the
-- subscription to cancel at period end, any pending prorations will
-- also be left in place and collected at the end of the period, but
-- if the subscription is set to cancel immediately, pending
-- prorations will be removed.
--
-- By default, all unpaid invoices for the customer will be closed
-- upon subscription cancellation. We do this in order to prevent
-- unexpected payment retries once the customer has canceled a
-- subscription. However, you can reopen the invoices manually after
-- subscription cancellation to have us proceed with automatic
-- retries, or you could even re-attempt payment yourself on all
-- unpaid invoices before allowing the customer to cancel the
-- subscription at all.
cancelSubscription :: CustomerId  -- ^ 'CustomerId'
                   -> Maybe Bool  -- ^ A flag that if set to true will delay the cancellation of the subscription until the end of the current period.
                   -> StripeReq Subscription
cancelSubscription cid mAtPeriodEnd =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId cid) ++ "/subscription"
              , srQueryString = maybe [] (\b -> [("at_period_end", boolBS b)]) mAtPeriodEnd
              , srMethod      = SDelete
              }
