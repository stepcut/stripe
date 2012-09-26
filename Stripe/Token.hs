{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
module Stripe.Token where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.ByteString     (ByteString)
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core         

------------------------------------------------------------------------------
-- CardInfo
------------------------------------------------------------------------------

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

cardInfoPairs :: CardInfo -> [(ByteString, ByteString)]
cardInfoPairs (CardInfo{..}) =
    catMaybes [ Just  ("card[number]", Text.encodeUtf8 cardInfoNumber)
              , Just  ("card[exp_month]", showBS cardInfoExpMonth)
              , Just  ("card[exp_year]", showBS cardInfoExpYear)
              , mbParam "card[cvc]"             cardInfoCvc         showBS 
              , mbParam "card[name]"            cardInfoName        Text.encodeUtf8 
              , mbParam "card[address_line1]"   cardInfoAddr1       Text.encodeUtf8 
              , mbParam "card[address_line2]"   cardInfoAddr2       Text.encodeUtf8 
              , mbParam "card[address_zip]"     cardInfoAddrZip     Text.encodeUtf8 
              , mbParam "card[address_state]"   cardInfoAddrState   Text.encodeUtf8 
              , mbParam "card[address_country]" cardInfoAddrCountry Text.encodeUtf8 
              ]

------------------------------------------------------------------------------
-- CardToken
------------------------------------------------------------------------------

newtype CardTokenId = CardTokenId { unCardTokenId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data CardToken = CardToken
    { cardTokenId       :: CardTokenId
    , cardTokenLivemode :: Bool
    , cardTokenCard     :: Card
    , cardTokenCreated  :: Timestamp
    , cardTokenUsed     :: Bool
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardToken)

createCardToken :: CardInfo
                -> StripeReq CardToken
createCardToken cardInfo =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens"
              , srQueryString = []
              , srMethod      = SPost (cardInfoPairs cardInfo)
              }

getCardToken :: CardTokenId
             -> StripeReq CardToken
getCardToken cti =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens/" ++ Text.unpack (unCardTokenId cti)
              , srQueryString = []
              , srMethod      = SGet
              }
