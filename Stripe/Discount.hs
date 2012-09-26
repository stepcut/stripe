{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
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
    { discountCoupon     :: Coupon
    , discountCustomerId :: CustomerId
    , discountStart      :: Timestamp
    , discountEnd        :: Maybe Timestamp
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


deleteDiscount :: CustomerId
               -> StripeReq Discount
deleteDiscount customerId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/customers/" ++ Text.unpack (unCustomerId customerId) ++ "/discount"
              , srQueryString = []
              , srMethod      = SDelete
              }

