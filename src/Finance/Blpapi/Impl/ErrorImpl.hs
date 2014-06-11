{-|
Module      : Finance.Blpapi.Impl.ErrorImpl
Description : FFI to extract Error
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.ErrorImpl where

import           Control.Monad
import           Foreign.C.String

foreign import ccall safe "blpapi_error.h blpapi_getLastErrorDescription"
    blpapi_getLastErrorDescription :: Int -> IO CString

getErrorString :: Int -> IO String
getErrorString rc =
    blpapi_getLastErrorDescription rc >>= peekCString


failIfBadErrorCode :: Int -> IO ()
failIfBadErrorCode rc =
    when (rc /= 0) $ getErrorString rc >>= fail
