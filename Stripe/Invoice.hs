{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe.Invoice where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core         
import Stripe.Discount     (Discount)
import Stripe.Plan         (Plan)

------------------------------------------------------------------------------
-- Invoice
------------------------------------------------------------------------------

newtype InvoiceId = InvoiceId { unInvoiceId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

newtype InvoiceItemId = InvoiceItemId { unInvoiceItemId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

data InvoiceItem = InvoiceItem
    { invoiceItemId          :: InvoiceItemId
    , invoiceItemLivemode    :: Bool
    , invoiceItemAmount      :: Cents
    , invoiceItemCurrency    :: Currency
    , invoiceItemCustomerId  :: CustomerId
    , invoiceItemDate        :: Timestamp
    , invoiceItemDescription :: Maybe Text
    , invoiceItemInvoiceId   :: InvoiceId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''InvoiceItem)

instance FromJSON InvoiceItem where
    parseJSON (Object obj) =
        InvoiceItem <$> obj .: "id"
                    <*> obj .: "livemode"
                    <*> obj .: "amount"
                    <*> obj .: "currency"
                    <*> obj .: "customer"
                    <*> obj .: "date"
                    <*> obj .: "description"
                    <*> obj .: "invoice"
    parseJSON _ = mzero

newtype InvoiceProrationId = InvoiceProrationId { unInvoiceProrationId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

data InvoiceProration = InvoiceProration
    { invoiceProrationId          :: InvoiceProrationId
    , invoiceProrationLivemode    :: Bool
    , invoiceProrationAmount      :: Cents
    , invoiceProrationCurrency    :: Currency
    , invoiceProrationCustomerId  :: CustomerId
    , invoiceProrationDate        :: Timestamp
    , invoiceProrationDescription :: Maybe Text
    , invoiceProrationInvoiceId   :: Maybe InvoiceId
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''InvoiceProration)

instance FromJSON InvoiceProration where
    parseJSON (Object obj) =
        InvoiceProration <$> obj .: "id"
                         <*> obj .: "livemode"
                         <*> obj .: "amount"
                         <*> obj .: "currency"
                         <*> obj .: "customer"
                         <*> obj .: "date"
                         <*> obj .: "description"
                         <*> obj .: "invoice"
    parseJSON _ = mzero

data Period = Period
    { end   :: Timestamp
    , start :: Timestamp
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Period)

instance FromJSON Period where
    parseJSON (Object obj) =
        Period <$> obj .: "end"
               <*> obj .: "start"
    parseJSON _ = mzero

data InvoiceSubscription = InvoiceSubscription
    { invoiceSubscriptionAmount   :: Cents
    , invoiceSubscriptionPeriod   :: Period
    , invoiceSubscriptionPlan     :: Plan
    , invoiceSubscriptionQuantity :: Integer
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''InvoiceSubscription)

instance FromJSON InvoiceSubscription where
    parseJSON (Object obj) =
        InvoiceSubscription <$> obj .: "amount"
                            <*> obj .: "period"
                            <*> obj .: "plan"
                            <*> obj .: "quantity"
    parseJSON _ = mzero

data InvoiceLines = InvoiceLines
    { invoiceItems         :: [InvoiceItem]
    , invoiceProrations    :: [InvoiceProration]
    , invoiceSubscriptions :: [InvoiceSubscription]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''InvoiceLines)

instance FromJSON InvoiceLines where
    parseJSON (Object obj) =
        InvoiceLines <$> obj .: "invoiceitems"
                     <*> obj .: "prorations"
                     <*> obj .: "subscriptions"
    parseJSON _ = mzero

data Invoice = Invoice
    { invoiceId                 :: InvoiceId
    , invoiceLivemode           :: Bool
    , invoiceAmountDue          :: Cents
    , invoiceAttemptCount       :: Integer
    , invoiceAttempted          :: Bool
    , invoiceClosed             :: Bool
    , invoiceCurrency           :: Currency
    , invoiceCustomerId         :: CustomerId
    , invoiceDate               :: Timestamp
    , invoiceLines              :: InvoiceLines
    , invoicePaid               :: Bool
    , invoicePeriodEnd          :: Timestamp
    , invoicePeriodStart        :: Timestamp
    , invoiceStartingBalance    :: Cents
    , invoiceSubTotal           :: Cents
    , invoiceTotal              :: Cents
    , invoiceCharge             :: Maybe Text
    , invoiceDiscount           :: Maybe Discount
    , invoiceEndingBalance      :: Maybe Integer
    , invoiceNextPaymentAttempt :: Maybe Timestamp
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Invoice)

instance FromJSON Invoice where
    parseJSON (Object obj) =
        Invoice <$> obj .: "id"
                <*> obj .: "livemode"
                <*> obj .: "amount_due"
                <*> obj .: "attempt_count"
                <*> obj .: "attempted"
                <*> obj .: "closed"
                <*> obj .: "currency"
                <*> obj .: "customer"
                <*> obj .: "date"
                <*> obj .: "lines"
                <*> obj .: "paid"
                <*> obj .: "period_end"
                <*> obj .: "period_start"
                <*> obj .: "starting_balance"
                <*> obj .: "subtotal"
                <*> obj .: "total"
                <*> obj .: "charge"
                <*> obj .: "discount"
                <*> obj .: "ending_balance"
                <*> obj .: "next_payment_attempt"
    parseJSON _ = mzero

getInvoice :: InvoiceId
           -> StripeReq Invoice
getInvoice iid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid)
              , srQueryString = []
              , srMethod      = SGet
              }

payInvoice :: InvoiceId
           -> Maybe Integer
           -> StripeReq Invoice
payInvoice iid mDepth =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid) ++ "/pay"
              , srQueryString = []
              , srMethod      = SPost (maybe [] (\d -> [("depth", showBS d)]) mDepth)
              }

updateInvoice :: InvoiceId
              -> Maybe Bool
              -> Maybe Integer
              -> StripeReq Invoice
updateInvoice iid mClosed mDepth =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid)
              , srQueryString = []
              , srMethod      = SPost $ catMaybes [ mbParam "closed" mClosed boolBS
                                                  , mbParam "depth"  mDepth  showBS
                                                  ]
              }

getInvoices :: Maybe CustomerId
            -> Maybe Count
            -> Maybe Offset
            -> StripeReq (List Invoice)
getInvoices mCustomerId mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices"
              , srQueryString = catMaybes [ mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                                          , mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }


getUpcomingInvoice :: CustomerId
                   -> StripeReq Invoice
getUpcomingInvoice cid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/incoming"
              , srQueryString = [ ("customer", Text.encodeUtf8 (unCustomerId cid)) ]
              , srMethod      = SGet
              }

------------------------------------------------------------------------------
-- InvoiceItem
------------------------------------------------------------------------------

createInvoiceItem :: CustomerId
                  -> Cents
                  -> Currency
                  -> Maybe InvoiceId
                  -> Maybe Text
                  -> StripeReq InvoiceItem
createInvoiceItem customerId amount currency mInvoiceId mDescription =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems"
              , srQueryString = []
              , srMethod      = SPost $ catMaybes [ Just ("customer", Text.encodeUtf8 (unCustomerId customerId))
                                                  , Just ("amount"  , showBS amount)
                                                  , Just ("currency", Text.encodeUtf8 currency)
                                                  , mbParam "invoice"     mInvoiceId (Text.encodeUtf8 . unInvoiceId)
                                                  , mbParam "description" mDescription Text.encodeUtf8
                                                  ]
              }

getInvoiceItem :: InvoiceId
               -> StripeReq InvoiceItem
getInvoiceItem invoiceId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SGet
              }

updateInvoiceItem :: InvoiceId
                  -> Maybe Cents
                  -> Maybe Text
                  -> StripeReq InvoiceItem
updateInvoiceItem invoiceId mAmount mDescription=
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SPost $ catMaybes [ mbParam "amount"      mAmount      showBS
                                                  , mbParam "description" mDescription Text.encodeUtf8
                                                  ]
              }

deleteInvoiceItem :: InvoiceId
                  -> StripeReq InvoiceItem
deleteInvoiceItem invoiceId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SDelete
              }

getInvoiceItems :: Maybe CustomerId
                -> Maybe Count
                -> Maybe Offset
                -> StripeReq (List InvoiceItem)
getInvoiceItems mCustomerId mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems"
              , srQueryString = catMaybes [ mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                                          , mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }

