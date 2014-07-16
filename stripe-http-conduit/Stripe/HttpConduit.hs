{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
{- |

Invoke a 'StripeReq' using @http-conduit@.

-}
module Stripe.HttpConduit where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Aeson          (FromJSON, eitherDecode')
import Data.Conduit
import Data.Maybe          (fromJust)
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as W
import Stripe.Core

------------------------------------------------------------------------------
-- stripe
------------------------------------------------------------------------------

stripe :: ( MonadResource m
          , MonadBaseControl IO m
          , FromJSON a
          ) =>
          ApiKey      -- ^ Stripe 'ApiKey'
       -> StripeReq a -- ^ request
       -> Manager     -- ^ conduit 'Manager'
       -> m (Either StripeError a)
stripe (ApiKey k) (StripeReq{..}) manager =
    do let req' = (fromJust $ parseUrl srUrl) { queryString     = W.renderSimpleQuery False srQueryString
                                              , responseTimeout = Just (60 * 10^6)
                                              }
           req = case srMethod of
                   SGet -> req'
                   (SPost params) -> urlEncodedBody params req'
                   SDelete        -> req' { method = "DELETE" }
       res <- httpLbs (applyBasicAuth k "" (req { checkStatus = \_ _ _ -> Nothing})) manager
--       liftIO $ print $ responseStatus res
--       liftIO $ putStrLn $ Text.unpack $ Text.decodeUtf8 $ toStrict $ responseBody  res
       if W.statusCode (responseStatus res) == 200
          then case eitherDecode' (responseBody res) of
                 (Left e)  -> return $ Left $ StripeAesonError e
                 (Right r) -> return $ Right r
          else case eitherDecode' (responseBody res) of
                 (Left e) -> return  $ Left $ StripeAesonError e
                 (Right r) -> return $ Left r
