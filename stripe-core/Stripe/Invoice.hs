{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
{- |

Invoices are statements of what a customer owes for a particular
billing period, including subscriptions, invoice items, and any
automatic proration adjustments if necessary.

Once an 'Invoice' is created, payment is automatically attempted. Note
that the payment, while automatic, does not happen exactly at the time
of invoice creation. If you have configured webhooks, the invoice will
wait until one hour after the last webhook is successfully sent (or
the last webhook times out after failing).

Any customer credit on the account is applied before determining how
much is due for that invoice (the amount that will be actually
charged). If the amount due for the invoice is less than 50 cents (the
minimum for a charge), we add the amount to the customer's running
account balance to be added to the next invoice. If this amount is
negative, it will act as a credit to offset the next invoice. Note
that the customer account balance does not include unpaid invoices; it
only includes balances that need to be taken into account when
calculating the amount due for the next invoice.

-}
module Stripe.Invoice where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy(..), base, contain, deriveSafeCopy, safeGet, safePut)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core
import Stripe.Discount     (Discount)
import Stripe.Plan         (Plan)

------------------------------------------------------------------------------
-- Invoice
------------------------------------------------------------------------------

-- | unique identifier for an 'Invoice'
newtype InvoiceId = InvoiceId { unInvoiceId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, FromJSON)

instance SafeCopy InvoiceId where
    kind = base
    getCopy = contain $ (InvoiceId . Text.decodeUtf8) <$> safeGet
    putCopy = contain . safePut . Text.encodeUtf8 . unInvoiceId
    errorTypeName _ = "Stripe.Invoice.InvoiceId"

-- | unique identifier for an 'InvoiceItem'
newtype InvoiceItemId = InvoiceItemId { unInvoiceItemId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, FromJSON)

instance SafeCopy InvoiceItemId where
    kind = base
    getCopy = contain $ (InvoiceItemId . Text.decodeUtf8) <$> safeGet
    putCopy = contain . safePut . Text.encodeUtf8 . unInvoiceItemId
    errorTypeName _ = "Stripe.Invoice.InvoiceItemId"

-- | Sometimes you want to add a charge or credit to a customer but
-- only actually charge the customer's card at the end of a regular
-- billing cycle. This is useful for combining several charges to
-- minimize per-transaction fees or having Stripe tabulate your
-- usage-based billing totals.
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
    deriving (Eq, Ord, Read, Show, Data, Typeable, FromJSON)

instance SafeCopy InvoiceProrationId where
    kind = base
    getCopy = contain $ (InvoiceProrationId . Text.decodeUtf8) <$> safeGet
    putCopy = contain . safePut . Text.encodeUtf8 . unInvoiceProrationId
    errorTypeName _ = "Stripe.Invoice.InvoiceProrationId"

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

-- | a time period
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

-- Invoices are statements of what a customer owes for a particular billing period, including subscriptions, invoice items, and any automatic proration adjustments if necessary.
--
-- The invoice object contains a lines hash that contains information
-- about the subscriptions and invoice items that have been applied to
-- the invoice, as well as any prorations that Stripe has
-- automatically calculated. Each line on the invoice has an amount
-- attribute that represents the amount actually contributed to the
-- invoice's total. For invoice items and prorations, the amount
-- attribute is the same as for the invoice item or proration
-- respectively. For subscriptions, the amount may be different from
-- the plan's regular price depending on whether the invoice covers a
-- trial period or the invoice period differs from the plan's usual
-- interval.
--
-- The invoice object has both a subtotal and a total. The subtotal
-- represents the total before any discounts, while the total is the
-- final amount to be charged to the customer after all coupons have
-- been applied.
--
-- The invoice also has a next_payment_attempt attribute that tells
-- you the next time (as a UTC timestamp) payment for the invoice will
-- be automatically attempted. For invoices that have been closed or
-- that have reached the maximum number of retries (specified in your
-- retry settings) , the next_payment_attempt will be null.
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

-- | Retrieves the 'Invoice' with the given ID.
getInvoice :: InvoiceId
           -> StripeReq Invoice
getInvoice iid =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid)
              , srQueryString = []
              , srMethod      = SGet
              }

-- | Stripe automatically creates and then attempts to pay invoices
-- for customers on subscriptions. We'll also retry unpaid invoices
-- according to your retry settings. However, if you'd like to attempt
-- to collect payment on an invoice out of the normal retry schedule
-- or for some other reason, you can do so.
payInvoice :: InvoiceId
           -> Maybe Integer -- ^ depth (whatever that is..)
           -> StripeReq Invoice
payInvoice iid mDepth =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid) ++ "/pay"
              , srQueryString = []
              , srMethod      = SPost (maybe [] (\d -> [("depth", showBS d)]) mDepth)
              }

-- | Until an invoice is paid, it is marked as open (closed=false). If
-- you'd like to stop Stripe from automatically attempting payment on
-- an invoice or would simply like to close the invoice out as no
-- longer owed by the customer, you can update the closed parameter.
updateInvoice :: InvoiceId
              -> Maybe Bool    -- ^ Boolean representing whether an invoice is closed or not. To close an invoice, pass true.
              -> Maybe Integer -- ^ Depth (whatever that is...)
              -> StripeReq Invoice
updateInvoice iid mClosed mDepth =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices/" ++ Text.unpack (unInvoiceId iid)
              , srQueryString = []
              , srMethod      = SPost $ catMaybes [ mbParam "closed" mClosed boolBS
                                                  , mbParam "depth"  mDepth  showBS
                                                  ]
              }

-- | You can list all invoices, or list the invoices for a specific customer.
getInvoices :: Maybe CustomerId -- ^ The identifier of the customer whose invoices to return. If none is provided, all invoices will be returned.
            -> Maybe Count      -- ^ A limit on the number of invoices to be returned. Count can range between 1 and 100 invoices. /default 10/
            -> Maybe Offset     -- ^ An offset into your invoices array. The API will return the requested number of invoices starting at that offset. /default 0/
            -> StripeReq (List Invoice)
getInvoices mCustomerId mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoices"
              , srQueryString = catMaybes [ mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                                          , mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }

-- | At any time, you can view the upcoming invoice for a
-- customer. This will show you all the charges that are pending,
-- including subscription renewal charges, invoice item charges,
-- etc. It will also show you any discount that is applicable to the
-- customer.
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

-- | Adds an arbitrary charge or credit to the customer's upcoming invoice.
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
-- | Retrieves the 'InvoiceItem' with the given ID.
getInvoiceItem :: InvoiceId
               -> StripeReq InvoiceItem
getInvoiceItem invoiceId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SGet
              }

-- | Updates the amount or description of an invoice item on an
-- upcoming invoice. Updating an invoice item is only possible before
-- the invoice it's attached to is closed.
updateInvoiceItem :: InvoiceId
                  -> Maybe Cents -- ^ The integer amount in cents of the charge to be applied to the upcoming invoice. If you want to apply a credit to the customer's account, pass a negative amount.
                  -> Maybe Text  -- ^ An arbitrary string which you can attach to the invoice item. The description is displayed in the invoice for easy tracking.
                  -> StripeReq InvoiceItem
updateInvoiceItem invoiceId mAmount mDescription=
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SPost $ catMaybes [ mbParam "amount"      mAmount      showBS
                                                  , mbParam "description" mDescription Text.encodeUtf8
                                                  ]
              }

-- | Removes an invoice item from the upcoming invoice. Removing an
-- invoice item is only possible before the invoice it's attached to
-- is closed.
deleteInvoiceItem :: InvoiceId
                  -> StripeReq InvoiceItem
deleteInvoiceItem invoiceId =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems/" ++ Text.unpack (unInvoiceId invoiceId)
              , srQueryString = []
              , srMethod      = SDelete
              }

-- | Returns a list of your invoice items. Invoice Items are returned
-- sorted by creation date, with the most recently created invoice
-- items appearing first.
getInvoiceItems :: Maybe CustomerId -- ^ The identifier of the customer whose invoice items to return. If none is provided, all invoice items will be returned.
                -> Maybe Count      -- ^ A limit on the number of invoice items to be returned. Count can range between 1 and 100 items. /default 10/
                -> Maybe Offset     -- ^ An offset into your invoice items array. The API will return the requested number of invoice items starting at that offset. /default 0/
                -> StripeReq (List InvoiceItem)
getInvoiceItems mCustomerId mCount mOffset =
    StripeReq { srUrl         = "https://api.stripe.com/v1/invoiceitems"
              , srQueryString = catMaybes [ mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                                          , mbParam "count"    mCount  showBS
                                          , mbParam "offset"   mOffset showBS
                                          ]
              , srMethod      = SGet
              }

