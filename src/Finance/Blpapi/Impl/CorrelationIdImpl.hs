{-|
Module      : Finance.Blpapi.Impl.CorrelationIdImpl
Description : FFI for CorrelationId
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.CorrelationIdImpl where

import           Foreign
import           Foreign.C.Types

import           Finance.Blpapi.CorrelationId

newtype CorrelationIdImpl = CorrelationIdImpl (Ptr CorrelationIdImpl)

data CorrelationIdImplWrapped = CorrelationIdImplWrapped {
      correlationValueW :: Int64,
      correlationClassW :: Int32,
      correlationTypeW  :: Int8
} deriving (Show, Eq)

kCorrelationTypeUnset :: Int8
kCorrelationTypeUnset = 0

kCorrelationTypeInt :: Int8
kCorrelationTypeInt = 1

kCorrelationTypePointer :: Int8
kCorrelationTypePointer = 2

kCorrelationTypeAutoGen :: Int8
kCorrelationTypeAutoGen = 3

instance Storable CorrelationIdImplWrapped where
    sizeOf    _ = 13
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        corValue <- peekByteOff ptr 0
        corClass <- peekByteOff ptr 8
        corType <- peekByteOff ptr 12
        return (CorrelationIdImplWrapped corValue corClass corType)

    poke ptr (CorrelationIdImplWrapped corValue corClass corType) =
            do
              pokeByteOff ptr 0 corValue
              pokeByteOff ptr 8 corClass
              pokeByteOff ptr 12 corType


foreign import ccall safe
  "blpapi_internal_wrapper.h blpapi_CorrelationId_convertToWrapped"
    blpapi_CorrelationId_convertToWrapped :: Ptr CorrelationIdImpl
                                            -> Ptr CorrelationIdImplWrapped
                                            -> IO ()

foreign import ccall safe
  "blpapi_internal_wrapper.h blpapi_CorrelationId_convertFromWrapped"
    blpapi_CorrelationId_convertFromWrapped :: Ptr CorrelationIdImplWrapped
                                            -> Ptr CorrelationIdImpl
                                            -> IO ()

foreign import ccall safe
  "blpapi_internal_wrapper.h blpapi_CorrelationId_create"
    blpapi_CorrelationId_create :: IO (Ptr CorrelationIdImpl)

foreign import ccall safe
  "blpapi_internal_wrapper.h &blpapi_CorrelationId_destroy"
    blpapi_CorrelationId_destroy :: FunPtr (Ptr CorrelationIdImpl -> IO ())

newCorrelationIdImpl :: IO (ForeignPtr CorrelationIdImpl)
newCorrelationIdImpl = do
  rawPtr <- blpapi_CorrelationId_create
  newForeignPtr blpapi_CorrelationId_destroy rawPtr

convertToCorrelationIdImpl :: CorrelationId -> CorrelationIdImplWrapped
convertToCorrelationIdImpl (CorrelationId UserGeneratedCid c v)
     = CorrelationIdImplWrapped v (fromIntegral c)
          (fromIntegral kCorrelationTypeInt)

convertToCorrelationIdImpl (CorrelationId InternallyGeneratedCid c v)
     = CorrelationIdImplWrapped
          v
          (fromIntegral c)
          (fromIntegral kCorrelationTypeAutoGen)

createUnsetCorrelationIdImpl :: CorrelationIdImplWrapped
createUnsetCorrelationIdImpl
    = CorrelationIdImplWrapped 0 0 kCorrelationTypeUnset

correlationIdImplConvertToType :: Int8 -> Int8 -> Int8
correlationIdImplConvertToType cidType classId
    = (cidType `shiftL` 8) .|. (classId `shiftL` 12)

convertFromCorrelationIdImpl :: CorrelationIdImplWrapped -> Maybe CorrelationId
convertFromCorrelationIdImpl (CorrelationIdImplWrapped v c t)
    | t == kCorrelationTypeInt
      = Just $ CorrelationId UserGeneratedCid (fromIntegral c) $ fromIntegral v
    | otherwise = Nothing


getCidTypeFromClass :: Int32 -> Int32
getCidTypeFromClass c = (c `shiftR` 8) .&. 0xf

getClassValuesFromClass :: Int32 -> Int16
getClassValuesFromClass c = fromIntegral $ (c `shiftR` 12) .&. 0xffff

