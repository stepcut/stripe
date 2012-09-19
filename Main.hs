{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import Network.HTTP.Conduit
import Stripe

testKey :: ApiKey
testKey = ApiKey "sk_TZjqQX1iWkWgMOuJivz4uElPsAkoI"

charges_1 :: IO ()
charges_1 =
    do res <- withManager $ stripe testKey (charges Nothing Nothing Nothing)
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

dummyCardToken :: CardToken
dummyCardToken = CardToken "tok_jOq0M8vJprCUUU"

create_charge_1 :: IO ()
create_charge_1 =
    do let sr@(StripeReq req) = createCharge 100 usd (CI dummyCard) (Just "a test charge")
       print $ method req
       print $ path req
       print $ queryString req 
       let (RequestBodyLBS lbs) = requestBody req
       putStrLn $ Text.unpack $ Text.decodeUtf8 lbs
       res <- withManager $ stripe testKey sr
       print res

