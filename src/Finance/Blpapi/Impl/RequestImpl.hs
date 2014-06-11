{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.RequestImpl
Description : FFI for Request
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.RequestImpl where

import           Foreign                 hiding (unsafePerformIO)

import           Finance.Blpapi.Impl.ElementImpl

newtype RequestImpl = RequestImpl (Ptr RequestImpl)

foreign import ccall safe "blpapi_request.h &blpapi_Request_destroy"
    blpapi_Request_destroy :: FunPtr(Ptr RequestImpl -> IO ())

foreign import ccall safe "blpapi_request.h blpapi_Request_elements"
    blpapi_Request_elements :: Ptr RequestImpl -> IO (Ptr ElementImpl)
