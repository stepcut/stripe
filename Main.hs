{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main where

import Data.Aeson
import Data.Attoparsec.ByteString (parseOnly)
import Data.String (IsString(fromString))
import Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Network.HTTP.Conduit
import Stripe.Core
import Stripe.Token
import Stripe.Charge
import Stripe.HttpConduit
import Verbatim

testKey :: ApiKey
testKey = ApiKey "sk_TZjqQX1iWkWgMOuJivz4uElPsAkoI"

charges_1 :: IO ()
charges_1 =
    do res <- withManager $ stripe testKey (getCharges Nothing Nothing Nothing)
       print res

dummyCard :: CardInfo
dummyCard = CardInfo
    { cardInfoNumber      = "4242424242424242"
    , cardInfoExpMonth    = 01
    , cardInfoExpYear     = 20
    , cardInfoCvc         = Nothing
    , cardInfoName        = Nothing
    , cardInfoAddr1       = Nothing
    , cardInfoAddr2       = Nothing
    , cardInfoAddrZip     = Nothing
    , cardInfoAddrState   = Nothing
    , cardInfoAddrCountry = Nothing
    }

invalidCard :: CardInfo
invalidCard = CardInfo
    { cardInfoNumber      = "4242424242424241"
    , cardInfoExpMonth    = 01
    , cardInfoExpYear     = 20
    , cardInfoCvc         = Nothing
    , cardInfoName        = Nothing
    , cardInfoAddr1       = Nothing
    , cardInfoAddr2       = Nothing
    , cardInfoAddrZip     = Nothing
    , cardInfoAddrState   = Nothing
    , cardInfoAddrCountry = Nothing
    }


dummyCardTokenId :: CardTokenId
dummyCardTokenId = CardTokenId "tok_jOq0M8vJprCUUU"

create_charge_1 :: IO ()
create_charge_1 =
    do let sr = createCharge 100 usd (CI dummyCard) (Just "a test charge")
       res <- withManager $ stripe testKey sr
       print res

create_charge_2 :: IO ()
create_charge_2 =
    do let sr = createCharge 100 usd (CI invalidCard) (Just "a test charge")
       res <- withManager $ stripe testKey sr
       print res

retrieve_charge_1 =
    do res <- withManager $ stripe testKey (getCharge (ChargeId "ch_0P5N4NiWnsDdhn"))
       print res


retrieve_charge_2 =
    do res <- withManager $ stripe testKey (getCharge (ChargeId "ch_5N4NiWnsDdhn"))
       print res


chargeObjectValue :: Value
chargeObjectValue =
    case parseOnly json chargeObjectText of
              (Right v) -> v

chargeObjectText :: (IsString s) => s
chargeObjectText = fromString
    [verbatim| 
{
  "amount": 100,
  "amount_refunded": 0,
  "created": 1348155631,
  "currency": "usd",
  "customer": null,
  "description": null,
  "disputed": false,
  "failure_message": null,
  "fee": 33,
  "id": "ch_0P046e22rg0Uzc",
  "invoice": null,
  "livemode": false,
  "object": "charge",
  "paid": true,
  "refunded": false,
  "card": {
    "address_city": null,
    "address_country": null,
    "address_line1": null,
    "address_line1_check": null,
    "address_line2": null,
    "address_state": null,
    "address_zip": null,
    "address_zip_check": null,
    "country": "US",
    "cvc_check": null,
    "exp_month": 1,
    "exp_year": 2020,
    "fingerprint": "32hxV0d6XQZY9cp7",
    "last4": "4242",
    "name": null,
    "object": "card",
    "type": "Visa"
  },
  "fee_details": [
    {
      "type": "stripe_fee",
      "amount": 33,
      "application": null,
      "currency": "usd",
      "description": "Stripe processing fees"
    }
  ]
}

    |]


cardValue :: Value
cardValue = case parseOnly json cardString of
              (Right v) -> v
    where
      cardString =
          (fromString
           [verbatim| {
    "address_city": null,
    "address_country": null,
    "address_line1": null,
    "address_line1_check": null,
    "address_line2": null,
    "address_state": null,
    "address_zip": null,
    "address_zip_check": null,
    "country": "US",
    "cvc_check": null,
    "exp_month": 1,
    "exp_year": 2020,
    "fingerprint": "32hxV0d6XQZY9cp7",
    "last4": "4242",
    "name": null,
    "object": "card",
    "type": "Visa"
  }|])

