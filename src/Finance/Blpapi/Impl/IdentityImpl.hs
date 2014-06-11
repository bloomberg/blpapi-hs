{-|
Module      : Finance.Blpapi.Impl.IdentityImpl
Description : FFI for Identity
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.IdentityImpl where

import           Finance.Blpapi.Impl.ServiceImpl
import           Foreign                 hiding (unsafePerformIO)
newtype IdentityImpl = Ptr IdentityImpl
type IdentityHandle = ForeignPtr IdentityImpl

foreign import ccall safe "blpapi_identity.h blpapi_Identity_isAuthorized"
  blpapi_Identity_isAuthorized :: Ptr IdentityImpl -> Ptr ServiceImpl -> IO Int

foreign import ccall safe "blpapi_identity.h &blpapi_Identity_release"
  blpapi_Identity_release :: FunPtr (Ptr IdentityImpl -> IO ())

