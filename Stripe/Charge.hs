{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe.Charge where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core         
import Stripe.Token

--- | A fee associate with a 'Charge'
data FeeDetail = FeeDetail
    { feeDetailAmount      :: Cents       -- ^ amount
    , feeDetailCurrency    :: Currency    -- ^ currency
    , feeDetailType        :: Text        -- ^ type
    , feeDetailApplication :: Maybe Text  -- ^ application
    , feeDetailDescription :: Maybe Text  -- ^ description
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''FeeDetail)

instance FromJSON FeeDetail where
    parseJSON (Object obj) =
        FeeDetail <$> obj .: "amount"
                  <*> obj .: "currency"
                  <*> obj .: "type"
                  <*> obj .: "application"
                  <*> obj .: "description"
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- Charge
------------------------------------------------------------------------------

newtype ChargeId = ChargeId { unChargeId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)
             
data Charge = Charge
    { chargeId             :: ChargeId    -- ^ charge id
    , chargeLivemode       :: Bool        -- ^ livemode
    , chargeAmount         :: Cents       -- ^ amount
    , chargeCard           :: Card        -- ^ card
    , chargeCreated        :: Timestamp   -- ^ created
    , chargeCurrency       :: Currency    -- ^ currency
    , chargeDisputed       :: Bool        -- ^ disputed (whether charge has been disputed by the customer)
    , chargeFee            :: Cents       -- ^ fee
    , chargeFeeDetails     :: [FeeDetail] -- ^ list of fees
    , chargePaid           :: Bool        -- ^ paid
    , chargeRefunded       :: Bool        -- ^ false for partial refund
    , chargeAmountRefunded :: Maybe Cents 
    , chargeCustomer       :: Maybe CustomerId
    , chargeDescription    :: Maybe Text
    , chargeFailureMessage :: Maybe Text
    , chargeInvoice        :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Charge)

instance FromJSON Charge where
    parseJSON (Object obj) =
        Charge <$> obj .: "id"
               <*> obj .: "livemode"
               <*> obj .: "amount"
               <*> obj .: "card"
               <*> obj .: "created"
               <*> obj .: "currency"
               <*> obj .: "disputed"
               <*> obj .: "fee"
               <*> obj .: "fee_details"
               <*> obj .: "paid"
               <*> obj .: "refunded"
               <*> obj .: "amount_refunded"
               <*> obj .: "customer"
               <*> obj .: "description"
               <*> obj .: "failure_message"
               <*> obj .: "invoice"
    parseJSON _ = mzero

data Charges = Charges
    { chargesCount :: Integer
    , chargesData  :: [Charge]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Charges)

instance FromJSON Charges where
    parseJSON (Object obj) =
        Charges <$> obj .: "count"
                <*> obj .: "data"
    parseJSON _ = mzero

-- | Returns a list of charges you've previously created. The charges
-- are returned in sorted order, with the most recent charges
-- appearing first.
getCharges :: Maybe Count      -- ^ limit number of charges returned. default is 10. 
           -> Maybe Offset     -- ^ offset into list of charges. default is 0.
           -> Maybe CustomerId -- ^ only return charges for this customer id
           -> StripeReq Charges 
getCharges mCount mOffset mCustomerId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges"
              , srQueryString = params
              , srMethod      = SGet
              }
    where
      params = catMaybes [ mbParam "count"    mCount      showBS
                         , mbParam "offset"   mOffset     showBS
                         , mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                         ]

data ChargeTo
    = CI CardInfo
    | CT CardTokenId
    | CS CustomerId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ChargeTo)

createCharge :: Cents  -- ^ amount
             -> Currency 
             -> ChargeTo
             -> Maybe Text -- ^ description
             -> StripeReq Charge
createCharge amount currency chargeTo description =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges"
              , srQueryString = []
              , srMethod      = SPost params
              }
    where
      params =
               [ ("amount", showBS amount)
               , ("currency", Text.encodeUtf8 currency)
               ] ++ 
               (case chargeTo of
                   (CS (CustomerId  ci)) ->
                       [ ("customer", Text.encodeUtf8 ci)]
                   (CT (CardTokenId ct)) ->
                       [ ("card", Text.encodeUtf8 ct)]
                   (CI ci) -> cardInfoPairs ci
               )

getCharge :: ChargeId
          -> StripeReq Charge
getCharge cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid)
              , srQueryString = []
              , srMethod      = SGet
              }


refundCharge :: ChargeId
             -> Maybe Cents
             -> StripeReq Charge
refundCharge cid mCents =
    StripeReq { srUrl         = "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid) ++ "/refund"
              , srQueryString = []
              , srMethod      = SPost $ case mCents of
                                            Nothing      -> []
                                            (Just cents) -> [("amount", showBS cents)]
              }

