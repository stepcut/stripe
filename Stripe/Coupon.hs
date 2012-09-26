{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
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

newtype CouponId = CouponId { unCouponId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

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

data Coupon = Coupon
    { couponId               :: CouponId
    , couponLivemode         :: Bool
    , couponDuration         :: Duration
    , couponPercentOff       :: Integer
    , couponDurationInMonths :: Maybe Integer
    , couponMaxRedemptions   :: Maybe Integer
    , couponRedeemBy         :: Maybe Timestamp
    , couponTimeRedeeded     :: Maybe Integer
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

createCoupon :: Maybe CouponId
             -> Integer -- ^ precent off (1 - 100)
             -> Duration
             -> Maybe Integer
             -> Maybe Integer
             -> Maybe Timestamp
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

getCoupon :: CouponId
          -> StripeReq Coupon
getCoupon couponId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons/" ++ Text.unpack (unCouponId couponId)
              , srQueryString = []
              , srMethod      = SGet
              }

deleteCoupon :: CouponId
             -> StripeReq Coupon
deleteCoupon couponId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons/" ++ Text.unpack (unCouponId couponId)
              , srQueryString = []
              , srMethod      = SDelete
              }

getCoupons :: Maybe Count
           -> Maybe Offset
           -> StripeReq (List Coupon)
getCoupons mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/coupons"
              , srQueryString = catMaybes [ mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }
