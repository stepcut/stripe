{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}
module Stripe.Account where

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (mzero)
import Data.Aeson          (FromJSON(..), Value(..), (.:))
import Data.Data           (Data, Typeable)
import Data.Maybe          (catMaybes)
import Data.SafeCopy       (SafeCopy, base, deriveSafeCopy)
import           Data.Text          as Text (Text, unpack)
import qualified Data.Text.Encoding as Text
import Stripe.Core         

------------------------------------------------------------------------------
-- Account
------------------------------------------------------------------------------

newtype AccountId = AccountId { unAccountId :: Text }
    deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, FromJSON)

data Account = Account
    { accountId :: AccountId
    , accountChargeEnabled :: Bool
    , accountCurrenciesSupported :: [Currency]
    , accountDetailsSubmitted :: Bool
    , accountEmail :: Maybe Text
    , accountStatementDescriptor :: Maybe Text
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)
$(deriveSafeCopy 0 'base ''Account)

instance FromJSON Account where
    parseJSON (Object obj) =
        Account <$> obj .: "id"
                <*> obj .: "change_enabled"
                <*> obj .: "currencies_supported"
                <*> obj .: "details_submitted"
                <*> obj .: "email"
                <*> obj .: "statement_descriptor"
    parseJSON _ = mzero

getAccount :: StripeReq Account
getAccount =
    StripeReq { srUrl         = "https://api.stripe.com/v1/account"
              , srQueryString = []
              , srMethod      = SGet
              }

