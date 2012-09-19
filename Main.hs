{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Conduit
import Stripe

testKey :: ApiKey
testKey = ApiKey "sk_TZjqQX1iWkWgMOuJivz4uElPsAkoI"

main :: IO ()
main =
    do res <- withManager $ stripe testKey (charges Nothing Nothing Nothing)
       print res

