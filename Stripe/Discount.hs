{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
{- |

A 'Discount' represents the actual application of a 'Coupon' to a
particular 'Customer'. It contains information about when the 'Discount'
began and when it will end.

-}
module Stripe.Discount where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core
import Stripe.Coupon       (Coupon)

------------------------------------------------------------------------------
-- Discount
------------------------------------------------------------------------------

data Discount = Discount
    { discountCoupon     :: Coupon           -- ^ the 'Coupon' applied to create this 'Discount'
    , discountCustomerId :: CustomerId       -- ^ 'CustomerId' this 'Discount' is applied to
    , discountStart      :: Timestamp        -- ^ Date that the coupon was applied
    , discountEnd        :: Maybe Timestamp  -- ^ If the coupon has a duration of once or repeating, the date that this discount will end. If the coupon used has a forever duration, this attribute will be null.
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Discount)

instance FromJSON Discount where
    parseJSON (Object obj) =
        Discount <$> obj .: "coupon"
                 <*> obj .: "customer"
                 <*> obj .: "start"
                 <*> obj .: "end"
    parseJSON _ = mzero

-- | Removes the currently applied discount on a customer.
deleteDiscount :: CustomerId
               -> StripeReq Discount
deleteDiscount customerId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId customerId) ++ "/discount"
              , srQueryString = []
              , srMethod      = SDelete
              }
