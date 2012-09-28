{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
{- |
/Card Tokens/

Often you want to be able to charge credit cards without having to
hold sensitive card information on your own servers. @Stripe.js@ makes
this easy in the browser, but you can use the same technique in other
environments with our card token API.

Card tokens can be created with your publishable API key, which can
safely be embedded in downloadable applications like iPhone and
Android apps. You can then use a token anywhere in our API that a card
is accepted. Note that tokens are not meant to be stored or used more
than once -- to store payment details for use later, you should create
a Customer object.

-}
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

-- | Used for submitting card information. Similar to but different
-- from 'Card'.
data CardInfo = CardInfo
    { cardInfoNumber      :: Text        -- ^ The card number, as a string without any separators.
    , cardInfoExpMonth    :: Int         -- ^ Two digit number representing the card's expiration month.
    , cardInfoExpYear     :: Int         -- ^ Two digit number representing the card's expiration month.
    , cardInfoCvc         :: Maybe Int   -- ^ /highly recommended/. Card security code
    , cardInfoName        :: Maybe Text  -- ^ Cardholder's full name.
    , cardInfoAddr1       :: Maybe Text  -- ^ Address Line 1
    , cardInfoAddr2       :: Maybe Text  -- ^ Address Line 2
    , cardInfoAddrZip     :: Maybe Text  -- ^ Address Zip Code
    , cardInfoAddrState   :: Maybe Text  -- ^ Address State
    , cardInfoAddrCountry :: Maybe Text  -- ^ Address Country
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardInfo)

-- | convets 'CardInfo' into a list of (key, value) pairs that can be used as @POST@ submission data
cardInfoPairs :: CardInfo
              -> [(ByteString, ByteString)]
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

-- | unique @id@ for a 'CardToken'
--
-- see: 'createCardToken'
newtype CardTokenId = CardTokenId { unCardTokenId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

-- | a single use token that can be used instead of a 'Card'. Can be
-- safely embedded in downloadable applications like iPhone and
-- Android apps.
data CardToken = CardToken
    { cardTokenId       :: CardTokenId
    , cardTokenLivemode :: Bool
    , cardTokenCard     :: Card         -- ^ card used to make the 'Charge'
    , cardTokenCreated  :: Timestamp    
    , cardTokenUsed     :: Bool         -- ^ Whether or not this token has already been used (tokens can be used only once)
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''CardToken)

-- |Creates a single use token that wraps the details of a credit
-- card. This token can be used in place of a credit card dictionary
-- with any API method. These tokens can only be used once: by
-- creating a new 'Charge' object, or attaching them to a 'Customer'.
createCardToken :: CardInfo
                -> StripeReq CardToken
createCardToken cardInfo =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens"
              , srQueryString = []
              , srMethod      = SPost (cardInfoPairs cardInfo)
              }

-- | Retrieves the card token with the given ID.
getCardToken :: CardTokenId
             -> StripeReq CardToken
getCardToken cti =
    StripeReq { srUrl         = "https://api.stripe.com/v1/tokens/" ++ Text.unpack (unCardTokenId cti)
              , srQueryString = []
              , srMethod      = SGet
              }
