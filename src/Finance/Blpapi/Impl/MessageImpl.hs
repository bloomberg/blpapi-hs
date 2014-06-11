{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.MessageImpl
Description : FFI for Message
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}


module Finance.Blpapi.Impl.MessageImpl where

import           Control.Monad
import           Foreign                               hiding (unsafePerformIO)
import           Foreign.C.String

import           Data.Ix
import           Data.Maybe
import qualified Data.Text                             as T
import           Finance.Blpapi.CorrelationId
import           Finance.Blpapi.Event
import           Finance.Blpapi.Impl.CorrelationIdImpl
import           Finance.Blpapi.Impl.ElementImpl
import           Finance.Blpapi.Impl.ServiceImpl

newtype MessageImpl = MessageImpl (Ptr MessageImpl)

foreign import ccall safe "blpapi_message.h blpapi_Message_typeString"
    blpapi_Message_typeString :: Ptr MessageImpl -> IO CString

foreign import ccall safe "blpapi_message.h blpapi_Message_topicName"
    blpapi_Message_topicName :: Ptr MessageImpl -> IO CString

foreign import ccall safe "blpapi_message.h blpapi_Message_service"
    blpapi_Message_service :: Ptr MessageImpl -> IO (Ptr ServiceImpl)

foreign import ccall safe "blpapi_message.h blpapi_Message_numCorrelationIds"
    blpapi_Message_numCorrelationIds :: Ptr MessageImpl -> IO Int

foreign import ccall safe
  "blpapi_internal_wrapper.h blpapi_Message_correlationId_improved"
    blpapi_Message_correlationId :: Ptr MessageImpl
                                 -> Ptr CorrelationIdImpl
                                 -> Int
                                 -> IO ()


foreign import ccall safe "blpapi_message.h blpapi_Message_elements"
    blpapi_Message_elements :: Ptr MessageImpl -> IO (Ptr ElementImpl)

foreign import ccall safe "blpapi_message.h blpapi_Message_fragmentType"
    blpapi_Message_fragmentType :: Ptr MessageImpl -> IO Int

convertMessageImpl :: Ptr MessageImpl -> IO Message
convertMessageImpl ptr = do
  topicName <- blpapi_Message_topicName ptr >>= peekCString
  sericePtr <- blpapi_Message_service ptr
  service <- if sericePtr == nullPtr
            then return Nothing
            else liftM Just $ convertServiceImpl sericePtr
  blpElem <- blpapi_Message_elements ptr >>= convertElementImpl
  fragmentType <- blpapi_Message_fragmentType ptr
  cids <- getAllCorrelationIds ptr
  return $ Message (T.pack topicName)
                   service
                   blpElem
                   (toEnum fragmentType)
                   cids

getAllCorrelationIds :: Ptr MessageImpl -> IO [CorrelationId]
getAllCorrelationIds m = do
  numCids <- blpapi_Message_numCorrelationIds m
  maybeCidList <- forM (range (0, numCids - 1)) $ \i ->
    alloca $ \cidPtr -> do
        cidImpl <- newCorrelationIdImpl
        withForeignPtr cidImpl $ \cidImplP -> do
            blpapi_Message_correlationId m cidImplP i
            blpapi_CorrelationId_convertToWrapped cidImplP cidPtr
            cid <- peek cidPtr
            return $ convertFromCorrelationIdImpl cid
  return $ catMaybes maybeCidList
