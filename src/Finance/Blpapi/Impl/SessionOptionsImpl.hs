{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.SessionOptionsImpl
Description : FFI for SessionOptions
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.SessionOptionsImpl where

import           Finance.Blpapi.SessionOptions
import           Foreign
import           Foreign.C.String

newtype SessionOptionsImpl = SessionOptionsImpl (Ptr SessionOptionsImpl)

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_create"
        create_session_options :: IO (Ptr SessionOptionsImpl)

foreign import ccall safe
  "blpapi_sessionoptions.h &blpapi_SessionOptions_destroy"
        destroy_session_options :: FunPtr (Ptr SessionOptionsImpl -> IO ())

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setServerAddress"
        session_options_setServerAddress :: Ptr SessionOptionsImpl
                                         -> CString
                                         -> Int
                                         -> Int
                                         -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setConnectTimeout"
        session_options_setConnectTimeout :: Ptr SessionOptionsImpl
                                          -> Int
                                          -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setDefaultServices"
        session_options_setDefaultService :: Ptr SessionOptionsImpl
                                          -> CString
                                          -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setDefaultSubscriptionService"
        session_options_setDefaultSubscriptionService
             :: Ptr SessionOptionsImpl -> CString -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setDefaultTopicPrefix"
        session_options_setDefaultTopicPrefix :: Ptr SessionOptionsImpl
                                              -> CString
                                              -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h \
      \ blpapi_SessionOptions_setAllowMultipleCorrelatorsPerMsg"
        session_options_setAllowMultipleCorrelatorsPerMsg
             :: Ptr SessionOptionsImpl -> Int -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setClientMode"
        session_options_setClientMode :: Ptr SessionOptionsImpl
                                      -> Int
                                      -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setMaxPendingRequests"
        session_options_setMaxPendingRequests :: Ptr SessionOptionsImpl
                                              -> Int
                                              -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setAutoRestartOnDisconnection"
        session_options_setAutoRestartOnDisconnection :: Ptr SessionOptionsImpl
                                                      -> Int
                                                      -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setAuthenticationOptions"
        session_options_setAuthenticationOptions :: Ptr SessionOptionsImpl
                                                 -> CString
                                                 -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setNumStartAttempts"
        session_options_setNumStartAttempts :: Ptr SessionOptionsImpl
                                            -> Int
                                            -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_setMaxEventQueueSize"
        session_options_setMaxEventQueueSize :: Ptr SessionOptionsImpl
                                             -> Int
                                             -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h \
      \ blpapi_SessionOptions_setSlowConsumerWarningLoWaterMark"
        session_options_setSlowConsumerWarningLoWaterMark
               :: Ptr SessionOptionsImpl -> Float -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h \
      \ blpapi_SessionOptions_setSlowConsumerWarningHiWaterMark"
        session_options_setSlowConsumerWarningHiWaterMark
               :: Ptr SessionOptionsImpl -> Float -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h \
      \ blpapi_SessionOptions_setDefaultKeepAliveInactivityTime"
        session_options_setDefaultKeepAliveInactivityTime
               :: Ptr SessionOptionsImpl -> Int -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h \
      \ blpapi_SessionOptions_setDefaultKeepAliveResponseTimeout"
        session_options_setDefaultKeepAliveResponseTimeout
               :: Ptr SessionOptionsImpl -> Int -> IO Int

foreign import ccall safe
  "blpapi_sessionoptions.h blpapi_SessionOptions_serverPort"
        session_options_serverPort :: Ptr SessionOptionsImpl -> IO Int


newSessionOptions :: IO (ForeignPtr SessionOptionsImpl)
newSessionOptions = do
  rawPtr <- create_session_options
  newForeignPtr destroy_session_options rawPtr

populateServerAddresses :: Ptr SessionOptionsImpl -> [ServerAddress] -> IO ()
populateServerAddresses sptr sAddList = work sptr sAddList 0
    where work ptr (x:xs) i = do
            withCString
              (serverAddressHost x)
              (\cs -> session_options_setServerAddress ptr cs
                (serverAddressPort x) i)
            work ptr xs (i+1)
          work _ [] _ = return ()

populateBool :: Ptr SessionOptionsImpl
             -> Bool
             -> (Ptr SessionOptionsImpl -> Int -> IO Int)
             -> IO Int
populateBool ptr True fn = fn ptr 1
populateBool ptr False fn = fn ptr 0

convertSessionOptionsImpl :: SessionOptions
                          -> IO (ForeignPtr SessionOptionsImpl)
convertSessionOptionsImpl s = do
  rawPtr <- create_session_options
  populateServerAddresses rawPtr (serverAddresses s)
  session_options_setConnectTimeout rawPtr (connectTimeoutMs s)
  withCString (defaultService s) (session_options_setDefaultService rawPtr)
  withCString (defaultSubscriptionService s)
                  (session_options_setDefaultSubscriptionService rawPtr)
  withCString (defaultTopicPrefix s)
                  (session_options_setDefaultTopicPrefix rawPtr)
  populateBool rawPtr (allowMultipleCorrelatorsPerMsg s)
               session_options_setAllowMultipleCorrelatorsPerMsg
  session_options_setClientMode rawPtr ((fromEnum . clientMode) s)
  session_options_setMaxPendingRequests rawPtr (maxPendingRequests s)
  populateBool rawPtr (autoRestartOnDisconnection s)
               session_options_setAutoRestartOnDisconnection
  withCString  (authenticationOptions s)
                   (session_options_setAuthenticationOptions rawPtr)
  session_options_setNumStartAttempts rawPtr (numStartAttempts s)
  session_options_setMaxEventQueueSize rawPtr (maxEventQueueSize s)
  session_options_setSlowConsumerWarningLoWaterMark rawPtr
                      (slowConsumerWarningLoWaterMark s)
  session_options_setSlowConsumerWarningHiWaterMark rawPtr
                      (slowConsumerWarningHiWaterMark s)
  session_options_setDefaultKeepAliveInactivityTime rawPtr
                      (keepAliveInactivityTimeSec s)
  session_options_setDefaultKeepAliveResponseTimeout rawPtr
                      (keepAliveResponseTimeoutSec s)
  newForeignPtr destroy_session_options rawPtr
