{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, TypeFamilies, RecordWildCards #-}
module Stripe where

import Control.Applicative ((<$>), (<*>))
import Control.Monad        (mzero)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Conduit
import Data.Data     (Data, Typeable)
import Data.Maybe
import Data.Monoid
import Data.SafeCopy (SafeCopy, base, deriveSafeCopy)
import Data.String   (fromString)
import Data.Text     as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Control.Monad.Trans.Control
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W

toStrict :: BL.ByteString -> ByteString
toStrict = mconcat . BL.toChunks

-- WARNING: only safe on ascii
showBS :: (Show a) => a -> ByteString
showBS = fromString . show

mbParam :: ByteString -> Maybe a -> (a -> ByteString) -> Maybe (ByteString, ByteString)
mbParam _ Nothing _ = Nothing
mbParam name (Just v) show' = Just (name, show' v)

usd :: Text
usd = "usd"

newtype ApiKey = ApiKey { unApiKey :: ByteString }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CardError
    = IncorrectCardNumber
    | InvalidExpiryMonth
    | InvalidExpiryYear
    | InvalidCvc
    | ExpiredCard
    | IncorrectCvc
    | CardDeclined
    | Missing
    | ProcessingError
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardError)

data ErrorType
    = InvalidRequestError
    | ApiError
    | CardError CardError
      deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ErrorType)

data StripeError = StripeError
    { errorType :: ErrorType
    , message   :: Text
    , param     :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''StripeError)

newtype CustomerId = CustomerId { unCustomerId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

data Check
    = Pass
    | Fail
    | Unchecked
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Check)

instance FromJSON Check where
    parseJSON (String str)
        | str == "pass"      = return Pass
        | str == "fail"      = return Fail
        | str == "unchecked" = return Unchecked
        | otherwise          = mzero
    parseJSON Null = return Unchecked
    parseJSON _    = mzero

data Card = Card
    { cardExpMonth       :: Int
    , cardExpYear        :: Int
    , cardFingerprint    :: Text
    , cardLast4          :: Text
    , cardType           :: Text
    , cardAddrCity       :: Maybe Text
    , cardAddrCountry    :: Maybe Text
    , cardAddrLine1      :: Maybe Text
    , cardAddrLine1Check :: Maybe Check
    , cardAddrLine2      :: Maybe Text
    , cardAddrState      :: Maybe Text
    , cardAddrZip        :: Maybe Text
    , cardAddrZipCheck   :: Maybe Check
    , cardCountry        :: Maybe Text
    , cardCvcCheck       :: Maybe Check
    , cardName           :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Card)

instance FromJSON Card where
    parseJSON (Object obj) =
        Card <$> obj .: "exp_month"
             <*> obj .: "exp_year"
             <*> obj .: "fingerprint"
             <*> obj .: "last4"
             <*> obj .: "type"
             <*> obj .: "address_city"
             <*> obj .: "address_country"
             <*> obj .: "address_line1"
             <*> obj .: "address_line1_check"
             <*> obj .: "address_line2"
             <*> obj .: "address_state"
             <*> obj .: "address_zip"
             <*> obj .: "address_zip_check"
             <*> obj .: "country"
             <*> obj .: "cvc_check"
             <*> obj .: "name"
    parseJSON _ = mzero

type Cents = Integer

data FeeDetail = FeeDetail
    { feeDetailAmount      :: Cents
    , feeDetailCurrency    :: Text
    , feeDetailType        :: Text
    , feeDetailApplication :: Maybe Text
    , feeDetailDescription :: Maybe Text
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

type Timestamp = Integer    

newtype ChargeId = ChargeId { unChargeId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)
             
data Charge = Charge
    { chargeId             :: ChargeId
    , chargeLivemode       :: Bool
    , chargeAmount         :: Cents
    , chargeCard           :: Card
    , chargeTimestamp      :: Timestamp
    , chargeCurrency       :: Text
    , chargeDisputed       :: Bool
    , chargeFee            :: Cents
    , chargeFeeDetails     :: [FeeDetail]
    , chargePaid           :: Bool
    , chargeRefunded       :: Bool -- false for partial refund
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

type Count = Integer
type Offset = Integer

newtype StripeReq m ret = StripeReq { unSR :: Request m }

charges :: Maybe Count -> Maybe Offset -> Maybe CustomerId -> StripeReq m Charges
charges mCount mOffset mCustomerId =
    let (Just req) = parseUrl "https://api.stripe.com/v1/charges"
    in StripeReq $ req { queryString = W.renderSimpleQuery False params  }
    where
      params = catMaybes [ mbParam "count" mCount (fromString . show)
                         , mbParam "offset" mOffset (fromString . show)
                         , mbParam "customer" mCustomerId (Text.encodeUtf8 . unCustomerId)
                         ]

type Currency = Text

data CardInfo = CardInfo
    { cardInfoNumber      :: Text
    , cardInfoExpMonth    :: Int
    , cardInfoExpYear     :: Int
    , cardInfoCvc         :: Maybe Int
    , cardInfoName        :: Maybe Text
    , cardInfoAddr1       :: Maybe Text
    , cardInfoAddr2       :: Maybe Text
    , cardInfoAddrZip     :: Maybe Text
    , cardInfoAddrState   :: Maybe Text
    , cardInfoAddrCountry :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardInfo)

newtype CardToken = CardToken { unCardToken :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data ChargeTo
    = CI CardInfo
    | CT CardToken
    | CS CustomerId
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''ChargeTo)

createCharge :: (Monad m)
             => Cents  -- ^ amount
             -> Currency 
             -> ChargeTo
             -> Maybe Text -- ^ description
             -> StripeReq m Charge
createCharge amount currency chargeTo description =
    let (Just req) = parseUrl "https://api.stripe.com/v1/charges"
    in StripeReq $ urlEncodedBody params req
    where
      params = -- catMaybes 
               [ ("amount", showBS amount)
               , ("currency", Text.encodeUtf8 currency)
               ] ++ 
               (case chargeTo of
                   (CS (CustomerId  ci)) ->
                       [ ("customer", Text.encodeUtf8 ci)]
                   (CT (CardToken ct)) ->
                       [ ("card", Text.encodeUtf8 ct)]
                   (CI (CardInfo{..})) ->
                       catMaybes [ Just  ("card[number]", Text.encodeUtf8 cardInfoNumber)
                                 , Just  ("card[exp_month]", showBS cardInfoExpMonth)
                                 , Just  ("card[exp_year]", showBS cardInfoExpYear)
                                 , mbItem "card[cvc]" showBS cardInfoCvc
                                 , mbItem "card[name]" Text.encodeUtf8 cardInfoName
                                 , mbItem "card[address_line1]" Text.encodeUtf8 cardInfoAddr1
                                 , mbItem "card[address_line2]" Text.encodeUtf8 cardInfoAddr2
                                 , mbItem "card[address_zip]" Text.encodeUtf8 cardInfoAddrZip
                                 , mbItem "card[address_state]" Text.encodeUtf8 cardInfoAddrState
                                 , mbItem "card[address_country]" Text.encodeUtf8 cardInfoAddrCountry
                                 ]
               )

mbItem k f Nothing = Nothing
mbItem k f (Just v) = Just (k, f v)

retrieveCharge :: (Monad m) => ChargeId -> StripeReq m Charge
retrieveCharge cid =
    let (Just req) = parseUrl $ "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid)
    in StripeReq req

refundCharge :: (Monad m) => ChargeId -> Maybe Cents -> StripeReq m Charge
refundCharge cid mCents =
    let (Just req) = parseUrl $ "https://api.stripe.com/v1/charges/" ++ (Text.unpack $ unChargeId cid) ++ "/refund"
    in case mCents of
         Nothing      -> StripeReq $ req { method = "POST" }
         (Just cents) -> StripeReq $ urlEncodedBody [("amount", showBS cents)] req
    
               
stripe :: ( MonadResource m
          , MonadBaseControl IO m
          , FromJSON a
          ) => ApiKey -> StripeReq m a -> Manager -> m (Either String (Maybe a))
stripe (ApiKey k) (StripeReq req) manager =
    do res <- httpLbs (applyBasicAuth k "" (req { checkStatus = \_ _ -> Nothing})) manager
       liftIO $ putStrLn $ Text.unpack $ Text.decodeUtf8 $ toStrict $ responseBody  res
       if W.statusCode (responseStatus res) == 200
          then return $ Right $ decode' (responseBody res)
          else return $ Left $ Text.unpack $ Text.decodeUtf8 $ toStrict $ responseBody  res
