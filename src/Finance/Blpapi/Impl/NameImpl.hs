{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.NameImpl
Description : FFI for Name
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.NameImpl where

import           Foreign          hiding (unsafePerformIO)
import           Foreign.C.String
import           System.IO.Unsafe (unsafePerformIO)

newtype NameImpl = NameImpl (Ptr NameImpl)

foreign import ccall safe "blpapi_name.h blpapi_Name_create"
    blpapi_Name_create :: CString -> IO (Ptr NameImpl)


foreign import ccall safe "blpapi_name.h blpapi_Name_destroy"
    blpapi_Name_destroy:: Ptr NameImpl -> IO ()

foreign import ccall safe "blpapi_name.h blpapi_Name_string"
    blpapi_Name_string:: Ptr NameImpl -> CString


foreign import ccall safe "blpapi_name.h blpapi_Name_findName"
    blpapi_Name_findName:: CString -> IO (Ptr NameImpl)


nameImplToString :: Ptr NameImpl -> String
nameImplToString ptr = unsafePerformIO $ peekCString (blpapi_Name_string ptr)

