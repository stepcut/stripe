{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
{- |

A 'Coupon' contains information about a percent-off 'Discount' you might
want to apply to a 'Customer'. Coupons only apply to 'Invoice's created
for recurring 'Subscription's and 'InvoiceItem's; they do not apply to
one-off 'Charge's.

-}
module Stripe.Coupon where

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
-- Coupon
------------------------------------------------------------------------------

-- | id uniquely identifying a 'Coupon'
newtype CouponId = CouponId { unCouponId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

-- | how long a 'Discount' is in effect
data Duration
    = Forever
    | Once
    | Repeating
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Duration)

instance FromJSON Duration where
    parseJSON (String str)
        | str == "forever"   = return Forever
        | str == "once"      = return Once
        | str == "repeating" = return Repeating
    parseJSON _ = mzero


-- | 'Coupon' object
data Coupon = Coupon
    { couponId               :: CouponId        -- ^ unique id
    , couponLivemode         :: Bool
    , couponDuration         :: Duration        -- ^ how long a customer who applies this coupon will get the discount.
    , couponPercentOff       :: Integer         -- ^ Percent that will be taken off the subtotal of any invoices for this customer for the duration of the coupon. For example, a coupon with percent_off of 50 will make a $100 invoice $50 instead.
    , couponDurationInMonths :: Maybe Integer   -- ^ If duration is repeating, the number of months the coupon applies. Null if coupon duration is forever or once.
    , couponMaxRedemptions   :: Maybe Integer   -- ^ Maximum number of times this coupon can be redeemed by a customer before it is no longer valid.
    , couponRedeemBy         :: Maybe Timestamp -- ^ Date after which the coupon can no longer be redeemed
    , couponTimeRedeeded     :: Maybe Integer   -- ^ Number of times this coupon has been applied to a customer.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Coupon)

instance FromJSON Coupon where
    parseJSON (Object obj) =
        Coupon <$> obj .: "id"
               <*> obj .: "livemode"
               <*> obj .: "duration"
               <*> obj .: "percent_off"
               <*> obj .: "duration_in_months"
               <*> obj .: "max_redemptions"
               <*> obj .: "redeem_by"
               <*> obj .: "times_redeemed"
    parseJSON _ = mzero

-- | You can create coupons easily via the coupon management page of
-- the Stripe dashboard. Coupon creation is also accessible via the
-- API if you need to create coupons on the fly.
createCoupon :: Maybe CouponId -- ^ Unique string of your choice that will be used to identify this coupon when applying it a customer. This is often a specific code you'll give to your customer to use when signing up (e.g. FALL25OFF). If you don't want to specify a particular code, you can leave the ID blank and we'll generate a random code for you.
             -> Integer         -- ^ A positive integer between 1 and 100 that represents the discount the coupon will apply.
             -> Duration        -- ^ Specifies how long the discount will be in effect. Can be forever, once, or repeating.
             -> Maybe Integer   -- ^ /required only if duration is repeating/. If duration is repeating, a positive integer that specifies the number of months the discount will be in effect.
             -> Maybe Integer   -- ^ A positive integer specifying the number of times the coupon can be redeemed before it's no longer valid. For example, you might have a 50% off coupon that the first 20 readers of your blog can use.
             -> Maybe Timestamp -- ^ UTC timestamp specifying the last time at which the coupon can be redeemed. After the redeem_by date, the coupon can no longer be applied to new customers.
             -> StripeReq Coupon
createCoupon mCouponId percentOff duration mDurationInMonths mMaxRedemptions mRedeemBy =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons"
              , srQueryString = []
              , srMethod      =
                  SPost $ (catMaybes
                           [ mbParam "coupon"             mCouponId    (showBS . unCouponId)
                           , Just $ ("percent_off", showBS percentOff)
                           , Just $ ("duraton", case duration of
                                                  Forever   -> "forever"
                                                  Once      -> "once"
                                                  Repeating -> "repeating")
                           , mbParam "duration_in_months" mDurationInMonths showBS
                           , mbParam "max_redemptions"    mMaxRedemptions   showBS
                           , mbParam "redeem_by"          mRedeemBy         showBS
                           ])
              }

-- | Retrieves the coupon with the given ID.
getCoupon :: CouponId
          -> StripeReq Coupon
getCoupon couponId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons/" ++ Text.unpack (unCouponId couponId)
              , srQueryString = []
              , srMethod      = SGet
              }

-- | You can delete coupons via the coupon management page of the
-- Stripe dashboard. However, deleting a coupon does not affect any
-- customers who have already applied the coupon; it means that new
-- customers can't redeem the coupon. You can also delete coupons via
-- the API.
deleteCoupon :: CouponId
             -> StripeReq Coupon
deleteCoupon couponId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons/" ++ Text.unpack (unCouponId couponId)
              , srQueryString = []
              , srMethod      = SDelete
              }

-- | Returns a list of your coupons.
getCoupons :: Maybe Count  -- ^ A limit on the number of coupons to be returned. Count can range between 1 and 100 items.
           -> Maybe Offset -- ^ An offset into your coupons array. The API will return the requested number of coupons starting at that offset.
           -> StripeReq (List Coupon)
getCoupons mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons"
              , srQueryString = catMaybes [ mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }
