{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.SubscriptionListImpl
Description : FFI for Subscriptions
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.SubscriptionListImpl where

import           Foreign                               hiding (unsafePerformIO)
import           Foreign.C.String

import           Finance.Blpapi.Impl.CorrelationIdImpl

newtype SubscriptionListImpl = SubscriptionListImpl (Ptr SubscriptionListImpl)

foreign import ccall safe
  "blpapi_subscriptionlist.h blpapi_SubscriptionList_create"
    blpapi_SubscriptionList_create :: IO (Ptr SubscriptionListImpl)

foreign import ccall safe
  "blpapi_subscriptionlist.h &blpapi_SubscriptionList_destroy"
    blpapi_SubscriptionList_destroy
      :: FunPtr (Ptr SubscriptionListImpl -> IO ())

foreign import ccall safe
  "blpapi_subscriptionlist.h blpapi_SubscriptionList_add"
  blpapi_SubscriptionList_add :: Ptr SubscriptionListImpl
                              -> CString -- subscription string
                              -> Ptr CorrelationIdImpl
                              -> Ptr CString -- fields
                              -> Ptr CString -- options
                              -> Int -- numFields
                              -> Int -- numOptions
                              -> IO Int

newSubscriptionListImpl :: IO (ForeignPtr SubscriptionListImpl)
newSubscriptionListImpl = do
  rawPtr <- blpapi_SubscriptionList_create
  newForeignPtr blpapi_SubscriptionList_destroy rawPtr

