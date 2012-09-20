{-# LANGUAGE QuasiQuotes #-}
module Verbatim where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib

verbatim :: QuasiQuoter
verbatim = QuasiQuoter
    { quoteExp  = \s -> stringE s
    , quotePat  = error "verbatim does not define quotePat"
    , quoteType = error "verbatim does not define quoteType"
    , quoteDec  = error "verbatim does not define quoteDec"
    }