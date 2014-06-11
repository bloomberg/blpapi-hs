{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.EventImpl
Description : FFI for Event
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.EventImpl where

import           Foreign                 hiding (unsafePerformIO)

import           Finance.Blpapi.Event
import           Finance.Blpapi.Impl.MessageImpl

newtype EventImpl = EventImpl (Ptr EventImpl)
newtype MessageIteratorImpl = MessageIteratorImpl (Ptr MessageIteratorImpl)

foreign import ccall safe "blpapi_event.h blpapi_Event_eventType"
    blpapi_Event_eventType:: Ptr EventImpl -> IO Int

foreign import ccall safe "blpapi_event.h blpapi_MessageIterator_create"
    blpapi_MessageIterator_create :: Ptr EventImpl 
                                  -> IO (Ptr MessageIteratorImpl)

foreign import ccall safe "blpapi_event.h blpapi_MessageIterator_destroy"
    blpapi_MessageIterator_destroy :: Ptr MessageIteratorImpl -> IO ()

foreign import ccall safe "blpapi_event.h blpapi_MessageIterator_next"
    blpapi_MessageIterator_next :: Ptr MessageIteratorImpl 
                                -> Ptr (Ptr MessageImpl)
                                -> IO Int

convertEventImpl :: Ptr EventImpl -> IO [Event]
convertEventImpl ptr = do
  currEventType <- blpapi_Event_eventType ptr
  iterator <- blpapi_MessageIterator_create ptr
  msgs <- getMessage iterator
  return $ map (Event (toEnum currEventType)) msgs


getMessage :: Ptr MessageIteratorImpl -> IO [Message]
getMessage ptr = alloca $ \m -> do
                   rc <- blpapi_MessageIterator_next ptr m
                   if rc == 0
                     then peek m >>= convertMessageImpl >>=
                            (\x -> do ml <- getMessage ptr ; return (x : ml))
                     else blpapi_MessageIterator_destroy ptr >> return []




