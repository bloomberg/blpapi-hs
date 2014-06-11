{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.ElementImpl
Description : FFI for Element
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.ElementImpl where

import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Finance.Blpapi.Event
import           Finance.Blpapi.Impl.DatetimeImpl
import           Finance.Blpapi.Impl.ErrorImpl
import           Finance.Blpapi.Impl.NameImpl
import           Finance.Blpapi.Impl.SchemaImpl

import qualified Finance.Blpapi.Impl.TypesImpl    as TI

import           Control.Monad
import           Foreign                          hiding (unsafePerformIO)
import           Foreign.C.String
import           Foreign.C.Types

newtype ElementImpl = ElementImpl (Ptr ElementImpl)

foreign import ccall safe "blpapi_element.h blpapi_Element_nameString"
    blpapi_Element_nameString :: Ptr ElementImpl -> IO CString

foreign import ccall safe "blpapi_element.h blpapi_Element_definition"
    blpapi_Element_definition :: Ptr ElementImpl
                              -> IO (Ptr SchemaDefinitionImpl)

foreign import ccall safe "blpapi_element.h blpapi_Element_datatype "
    blpapi_Element_datatype :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_isComplexType"
    blpapi_Element_isComplexType :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_isReadOnly"
    blpapi_Element_isReadOnly :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_isArray"
    blpapi_Element_isArray :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_numValues"
    blpapi_Element_numValues :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_numElements"
    blpapi_Element_numElements :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_isNull"
    blpapi_Element_isNull :: Ptr ElementImpl -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_isNullValue"
    blpapi_Element_isNullValue :: Ptr ElementImpl -> Int -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getElementAt"
    blpapi_Element_getElementAt :: Ptr ElementImpl
                                -> Ptr (Ptr ElementImpl)
                                -> Int
                                -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getElement"
    blpapi_Element_getElement :: Ptr ElementImpl
                              -> Ptr (Ptr ElementImpl)
                              -> CString -> Ptr NameImpl
                              -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsBool"
    blpapi_Element_getValueAsBool :: Ptr ElementImpl
                                  -> Ptr Bool
                                  -> Int
                                  -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsChar"
    blpapi_Element_getValueAsChar :: Ptr ElementImpl
                                  -> Ptr Char
                                  -> Int
                                  -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsInt32"
    blpapi_Element_getValueAsInt32 :: Ptr ElementImpl
                                   -> Ptr Int32
                                   -> Int
                                   -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsInt64"
    blpapi_Element_getValueAsInt64 :: Ptr ElementImpl
                                   -> Ptr Int64
                                   -> Int
                                   -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsFloat32"
    blpapi_Element_getValueAsFloat32 :: Ptr ElementImpl
                                     -> Ptr Float
                                     -> Int
                                     -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsFloat64"
    blpapi_Element_getValueAsFloat64 :: Ptr ElementImpl
                                     -> Ptr Double
                                     -> Int
                                     -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsString"
    blpapi_Element_getValueAsString :: Ptr ElementImpl
                                    -> Ptr CString
                                    -> Int
                                    -> IO Int

foreign import ccall safe
        "blpapi_element.h blpapi_Element_getValueAsHighPrecisionDatetime"
    blpapi_Element_getValueAsHighPrecisionDatetime :: Ptr ElementImpl
                                                   -> HPDatetimeImpl
                                                   -> Int
                                                   -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_getValueAsElement"
    blpapi_Element_getValueAsElement :: Ptr ElementImpl
                                     -> Ptr (Ptr ElementImpl)
                                     -> Int
                                     -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueBool"
    blpapi_Element_setValueBool :: Ptr ElementImpl -> Bool -> CInt -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueChar"
    blpapi_Element_setValueChar :: Ptr ElementImpl -> Char -> CInt -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueInt32"
    blpapi_Element_setValueInt32 :: Ptr ElementImpl -> Int -> CInt -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueInt64"
    blpapi_Element_setValueInt64 :: Ptr ElementImpl -> Int64 -> CInt -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueFloat32"
    blpapi_Element_setValueFloat32 :: Ptr ElementImpl
                                   -> Float
                                   -> CInt
                                   -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueFloat64"
    blpapi_Element_setValueFloat64 :: Ptr ElementImpl
                                   -> Double
                                   -> CInt
                                   -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueString"
    blpapi_Element_setValueString :: Ptr ElementImpl
                                  -> CString
                                  -> CInt
                                  -> IO Int

foreign import ccall safe
      "blpapi_element.h blpapi_Element_setValueHighPrecisionDatetime"
    blpapi_Element_setValueHighPrecisionDatetime :: Ptr ElementImpl
                                                 -> HPDatetimeImpl
                                                 -> Int
                                                 -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_setValueFromElement"
    blpapi_Element_setValueFromElement :: Ptr ElementImpl
                                       -> Ptr ElementImpl
                                       -> Int
                                       -> IO Int

foreign import ccall safe "blpapi_element.h blpapi_Element_appendElement"
    blpapi_Element_appendElement :: Ptr ElementImpl
                                 -> Ptr (Ptr ElementImpl)
                                 -> IO Int

convertElementImpl :: Ptr ElementImpl -> IO Element
convertElementImpl ptr =  do
    (name, elemO) <- convertElementImpl' ptr
    return $ ElementSequence $ Map.singleton name elemO

convertElementImpl' :: Ptr ElementImpl -> IO (T.Text, ElementWithDefinition)
convertElementImpl' ptr = do
  name <- blpapi_Element_nameString ptr >>= peekCString
  def <- blpapi_Element_definition ptr >>= convertSchemaDefinitionImpl
  isNull <- blpapi_Element_isNull ptr
  newElem <- if 0 == isNull
             then getElementType ptr
             else return ElementNull
  return (T.pack name, ElementWithDefinition def newElem)

getElementType :: Ptr ElementImpl -> IO Element
getElementType ptr = do
  isArray <- blpapi_Element_isArray ptr
  if isArray /= 0
    then getElementTypeImplArray ptr
    else getElementTypeImpl ptr


getElementTypeImplArray :: Ptr ElementImpl -> IO Element
getElementTypeImplArray ptr = do
  dataType <- blpapi_Element_datatype ptr
  if toEnum dataType == TI.BlpTypeSeq || toEnum dataType == TI.BlpTypeChoice
  then getElementTypeImplArrayComplex ptr
  else getElementTypeImplArraySimple ptr

getElementTypeImplArraySimple :: Ptr ElementImpl -> IO Element
getElementTypeImplArraySimple ptr = do
    numValues <- blpapi_Element_numValues ptr
    dataType <- blpapi_Element_datatype ptr
    lst <- forM [0..(numValues -1)]
        (\v -> getValueTypeSingle ptr v (toEnum dataType))
    return $ ElementArray lst

getElementTypeImplArrayComplex :: Ptr ElementImpl -> IO Element
getElementTypeImplArrayComplex ptr = do
  numValues <- blpapi_Element_numValues ptr
  dataType <- blpapi_Element_datatype ptr
  lst <- forM [0..(numValues -1)]
      (\index -> do
        arrElem <- alloca $ \v -> do
           rc <- blpapi_Element_getValueAsElement ptr v index
           failIfBadErrorCode rc
           peek v
        getValueTypeSingle arrElem 0 (toEnum dataType))
  return $ ElementArray lst


getElementTypeImpl :: Ptr ElementImpl -> IO Element
getElementTypeImpl ptr =
    getElementTypeSingle ptr 0

getElementTypeSingle :: Ptr ElementImpl -> Int -> IO Element
getElementTypeSingle ptr i = do
  dataType <- blpapi_Element_datatype ptr
  getValueTypeSingle ptr i (toEnum dataType)

getSubElementDataType :: Ptr ElementImpl -> Int -> IO TI.BlpapiDataTypes
getSubElementDataType ptr i = alloca $ \subElemPtr -> do
   rc <- blpapi_Element_getElementAt ptr subElemPtr i
   failIfBadErrorCode rc
   subElem <- peek subElemPtr
   dataType <- blpapi_Element_datatype subElem
   return $ toEnum dataType

getValueTypeSingle :: Ptr ElementImpl
                   -> Int
                   -> TI.BlpapiDataTypes
                   -> IO Element
getValueTypeSingle ptr index TI.BlpTypeBool =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsBool ptr v index
        failIfBadErrorCode rc
        liftM ElementBool $ peek v

getValueTypeSingle ptr index TI.BlpTypeChar =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsChar ptr v index
        failIfBadErrorCode rc
        liftM ElementChar $ peek v

getValueTypeSingle _ _ TI.BlpTypeByte = undefined

getValueTypeSingle ptr index TI.BlpTypeInt32 =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsInt32 ptr v index
        failIfBadErrorCode rc
        liftM (ElementInt . fromIntegral) (peek v)

getValueTypeSingle ptr index TI.BlpTypeInt64 =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsInt64 ptr v index
        failIfBadErrorCode rc
        liftM (ElementInt64 . fromIntegral) (peek v)

getValueTypeSingle ptr index TI.BlpTypeFloat32 =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsFloat32 ptr v index
        failIfBadErrorCode rc
        liftM ElementFloat (peek v)

getValueTypeSingle ptr index TI.BlpTypeFloat64 =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsFloat64 ptr v index
        failIfBadErrorCode rc
        liftM ElementDouble (peek v)

getValueTypeSingle ptr index TI.BlpTypeString =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsString ptr v index
        failIfBadErrorCode rc
        liftM (ElementString . T.pack) (peek v >>= peekCString)

getValueTypeSingle _ _ TI.BlpTypeByteArray = undefined

getValueTypeSingle ptr index TI.BlpTypeDate =
    getValueTypeSingle ptr index TI.BlpTypeDatetime

getValueTypeSingle ptr index TI.BlpTypeTime =
    getValueTypeSingle ptr index TI.BlpTypeDatetime

getValueTypeSingle _ _ TI.BlpTypeDecimal = undefined

getValueTypeSingle ptr index TI.BlpTypeDatetime =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsHighPrecisionDatetime ptr v index
        failIfBadErrorCode rc
        d <- peek v
        return $! getElementForDateOrTime d
        --return $! ElementDatetime $! convertBlpapiHPDatimeImpl d

getValueTypeSingle ptr index TI.BlpTypeEnum =
    alloca $ \v -> do
        rc <- blpapi_Element_getValueAsString ptr v index
        failIfBadErrorCode rc
        liftM ElementEnum (peek v >>= peekCString)

getValueTypeSingle ptr _ TI.BlpTypeSeq =
    alloca $ \v -> do
        n <- blpapi_Element_numElements ptr
        elemList <- forM [0..(n-1)] $ \index ->
          getSubElement ptr v index
        return $ ElementSequence $ Map.fromList elemList

getValueTypeSingle ptr index TI.BlpTypeChoice =
    alloca $ \v -> do
        (name, newElem) <- getSubElement ptr v index
        return $ ElementChoice $ Map.singleton name newElem


getSubElement :: Ptr ElementImpl
              -> Ptr (Ptr ElementImpl)
              -> Int
              -> IO (T.Text, ElementWithDefinition)
getSubElement ptr v index= do
        rc <- blpapi_Element_getElementAt ptr v index
        failIfBadErrorCode rc
        subElem <- peek v
        convertElementImpl' subElem


getElementForDateOrTime :: BlpapiHPDatetime -> Element
getElementForDateOrTime hpDate = case d' of
    BlpLocalTimeImpl d -> ElementDatetime d z
    BlpDateImpl d -> ElementDate d z
    BlpTimeImpl d -> ElementTime d z
  where
    (d', z) = convertBlpapiHPDatimeImpl hpDate

