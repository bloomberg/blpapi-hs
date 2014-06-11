{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.ServiceImpl
Description : FFI for Service
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.ServiceImpl where

import           Control.Monad
import           Foreign                         hiding (unsafePerformIO)
import           Foreign.C.String

import           Finance.Blpapi.Impl.ErrorImpl
import           Finance.Blpapi.Impl.RequestImpl
import           Finance.Blpapi.Impl.SchemaImpl
import           Finance.Blpapi.Service

newtype OperationImpl = OperationImpl (Ptr OperationImpl)
newtype ServiceImpl = ServiceImpl (Ptr ServiceImpl)

foreign import ccall safe "blpapi_service.h blpapi_Operation_name"
    blpapi_Operation_name :: Ptr OperationImpl -> IO CString

foreign import ccall safe "blpapi_service.h blpapi_Operation_description"
    blpapi_Operation_description :: Ptr OperationImpl -> IO CString

foreign import ccall safe "blpapi_service.h blpapi_Operation_requestDefinition"
    blpapi_Operation_requestDefinition :: Ptr OperationImpl
                                       -> Ptr (Ptr SchemaDefinitionImpl)
                                       -> IO Int

foreign import ccall safe
  "blpapi_service.h blpapi_Operation_numResponseDefinitions"
    blpapi_Operation_numResponseDefinitions :: Ptr OperationImpl -> IO Int

foreign import ccall safe
  "blpapi_service.h blpapi_Operation_responseDefinition"
    blpapi_Operation_responseDefinition:: Ptr OperationImpl
                                       -> Ptr (Ptr SchemaDefinitionImpl)
                                       -> Int
                                       -> IO Int

foreign import ccall safe "blpapi_service.h blpapi_Service_name"
    blpapi_Service_name :: Ptr ServiceImpl -> IO CString

foreign import ccall safe "blpapi_service.h blpapi_Service_description"
    blpapi_Service_description :: Ptr ServiceImpl -> IO CString

foreign import ccall safe "blpapi_service.h blpapi_Service_numOperations"
    blpapi_Service_numOperations :: Ptr ServiceImpl -> IO Int


foreign import ccall safe "blpapi_service.h blpapi_Service_numEventDefinitions"
    blpapi_Service_numEventDefinitions :: Ptr ServiceImpl -> IO Int


foreign import ccall safe "blpapi_service.h blpapi_Service_addRef"
    blpapi_Service_addRef :: Ptr ServiceImpl -> IO Int


foreign import ccall safe "blpapi_service.h &blpapi_Service_release"
    blpapi_Service_release :: FunPtr (Ptr ServiceImpl -> IO ())


foreign import ccall safe
  "blpapi_service.h blpapi_Service_authorizationServiceName"
    blpapi_Service_authorizationServiceName :: Ptr ServiceImpl -> IO CString

foreign import ccall safe "blpapi_service.h blpapi_Service_getOperationAt"
    blpapi_Service_getOperationAt :: Ptr ServiceImpl
                                  -> Ptr (Ptr OperationImpl)
                                  -> Int
                                  -> IO Int

foreign import ccall safe
  "blpapi_service.h blpapi_Service_getEventDefinitionAt"
    blpapi_Service_getEventDefinitionAt :: Ptr ServiceImpl
                                        -> Ptr (Ptr SchemaDefinitionImpl)
                                        -> Int
                                        -> IO Int

foreign import ccall safe "blpapi_service.h blpapi_Service_createRequest"
    blpapi_Service_createRequest :: Ptr ServiceImpl
                                 -> Ptr (Ptr RequestImpl)
                                 -> CString
                                 -> IO Int

foreign import ccall safe
  "blpapi_service.h blpapi_Service_createAuthorizationRequest"
    blpapi_Service_createAuthorizationRequest :: Ptr ServiceImpl
                                              -> Ptr (Ptr RequestImpl)
                                              -> CString
                                              -> IO Int

convertOperationImpl :: Ptr OperationImpl -> IO Operation
convertOperationImpl ptr = do
  name <- blpapi_Operation_name ptr >>= peekCString
  des <- blpapi_Operation_description ptr >>= peekCString
  req <- alloca $ \reqHandle -> do
                   rc <- blpapi_Operation_requestDefinition ptr reqHandle
                   failIfBadErrorCode rc
                   imp <- peek reqHandle
                   convertSchemaDefinitionImpl imp
  num <- blpapi_Operation_numResponseDefinitions ptr
  responseList <- forM [0..(num-1)] (responseDefintion ptr)
  return $ Operation name des req responseList
      where
        responseDefintion p ind = alloca $ \resHandle -> do
                   rc <- blpapi_Operation_responseDefinition p resHandle ind
                   failOrConvert rc resHandle

failOrConvert :: Int -> Ptr (Ptr SchemaDefinitionImpl) -> IO SchemaDefinition
failOrConvert rc resHandle = do
                   failIfBadErrorCode rc
                   imp <- peek resHandle
                   convertSchemaDefinitionImpl imp

convertServiceImpl :: Ptr ServiceImpl -> IO Service
convertServiceImpl ptr = do
  name <- blpapi_Service_name ptr >>= peekCString
  des <- blpapi_Service_description ptr >>= peekCString
  authServiceName <- blpapi_Service_authorizationServiceName ptr
                         >>= peekCString
  numOp <- blpapi_Service_numOperations ptr
  numSer <- blpapi_Service_numEventDefinitions ptr
  opList <- forM [0..(numOp-1)] $ opDefinition ptr
  evList <- forM [0..(numSer-1)] $ evDefinition ptr
  return $ Service name des authServiceName opList evList
      where
        opDefinition p ind = alloca $ \resHandle -> do
                    rc <- blpapi_Service_getOperationAt p resHandle ind
                    failIfBadErrorCode rc
                    imp <- peek resHandle
                    convertOperationImpl imp
        evDefinition p ind = alloca $ \resHandle -> do
                    rc <- blpapi_Service_getEventDefinitionAt p resHandle ind
                    failOrConvert rc resHandle


