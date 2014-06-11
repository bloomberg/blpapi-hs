{-|
Module      : Finance.Blpapi.Request
Description : Request Type
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Request where

import           Finance.Blpapi.Impl.RequestImpl

import           Foreign                 hiding (unsafePerformIO)

newtype Request = Request {
    getImpl :: ForeignPtr RequestImpl
}
