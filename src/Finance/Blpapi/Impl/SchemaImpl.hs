{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.SchemaImpl
Description : FFI for Schema
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.SchemaImpl where

import           Control.Monad
import           Foreign                          hiding (unsafePerformIO)
import           Foreign.C.String
import           Foreign.C.Types

import qualified Data.Text                        as Text
import           Finance.Blpapi.Impl.DatetimeImpl
import           Finance.Blpapi.Impl.NameImpl
import qualified Finance.Blpapi.Impl.TypesImpl    as TI
import           Finance.Blpapi.Service
import qualified Finance.Blpapi.Types             as T

newtype SchemaImpl = SchemaImpl (Ptr SchemaImpl)
newtype SchemaDefinitionImpl = SchemaDefinitionImpl (Ptr SchemaDefinitionImpl)
newtype SchemaTypeDefinitionImpl
    = SchemaTypeDefinitionImpl (Ptr SchemaTypeDefinitionImpl)

newtype ConstantListImpl = ConstantListImpl (Ptr ConstantListImpl)
newtype ConstantImpl = ConstantImpl (Ptr ConstantImpl)

foreign import ccall safe "blpapi_schema.h blpapi_SchemaElementDefinition_name"
    blpapi_SchemaElementDefinition_name :: Ptr SchemaDefinitionImpl
                                        -> IO (Ptr NameImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_description"
    blpapi_SchemaElementDefinition_description :: Ptr SchemaDefinitionImpl
                                               -> IO CString

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_status"
    blpapi_SchemaElementDefinition_status :: Ptr SchemaDefinitionImpl -> IO Int

foreign import ccall safe "blpapi_schema.h blpapi_SchemaElementDefinition_type"
    blpapi_SchemaElementDefinition_type :: Ptr SchemaDefinitionImpl
                                        -> IO (Ptr SchemaTypeDefinitionImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_numAlternateNames"
     blpapi_SchemaElementDefinition_numAlternateNames
      :: Ptr SchemaDefinitionImpl -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_getAlternateName"
    blpapi_SchemaElementDefinition_getAlternateName :: Ptr SchemaDefinitionImpl
                                                    -> Int
                                                    -> IO (Ptr NameImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_minValues"
    blpapi_SchemaElementDefinition_minValues :: Ptr SchemaDefinitionImpl
                                             -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaElementDefinition_maxValues"
    blpapi_SchemaElementDefinition_maxValues :: Ptr SchemaDefinitionImpl
                                             -> IO Int

-- TypeDefintion

foreign import ccall safe "blpapi_schema.h blpapi_SchemaTypeDefinition_name"
    blpapi_SchemaTypeDefinition_name :: Ptr SchemaTypeDefinitionImpl
                                     -> IO (Ptr NameImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_description"
    blpapi_SchemaTypeDefinition_description :: Ptr SchemaTypeDefinitionImpl
                                            -> IO CString

foreign import ccall safe "blpapi_schema.h blpapi_SchemaTypeDefinition_status"
    blpapi_SchemaTypeDefinition_status:: Ptr SchemaTypeDefinitionImpl -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_datatype"
    blpapi_SchemaTypeDefinition_datatype :: Ptr SchemaTypeDefinitionImpl
                                         -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_isComplexType"
    blpapi_SchemaTypeDefinition_isComplexType :: Ptr SchemaTypeDefinitionImpl
                                              -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_isSimpleType"
    blpapi_SchemaTypeDefinition_isSimpleType :: Ptr SchemaTypeDefinitionImpl
                                             -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_isEnumerationType"
    blpapi_SchemaTypeDefinition_isEnumerationType
     :: Ptr SchemaTypeDefinitionImpl -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_numElementDefinitions"
    blpapi_SchemaTypeDefinition_numElementDefinitions
      :: Ptr SchemaTypeDefinitionImpl -> IO Int

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_getElementDefinition"
    blpapi_SchemaTypeDefinition_getElementDefinition
      :: Ptr SchemaTypeDefinitionImpl
      -> CString
      -> Ptr NameImpl
      -> IO (Ptr SchemaDefinitionImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_getElementDefinitionAt"
    blpapi_SchemaTypeDefinition_getElementDefinitionAt
      :: Ptr SchemaTypeDefinitionImpl -> Int -> IO (Ptr SchemaDefinitionImpl)

foreign import ccall safe
  "blpapi_schema.h blpapi_SchemaTypeDefinition_enumeration"
    blpapi_SchemaTypeDefinition_enumeration
      :: Ptr SchemaTypeDefinitionImpl -> IO (Ptr ConstantListImpl)

foreign import ccall safe "blpapi_constant.h blpapi_Constant_name"
    blpapi_Constant_name:: Ptr ConstantImpl -> IO (Ptr NameImpl)

foreign import ccall safe "blpapi_constant.h blpapi_Constant_description"
    blpapi_Constant_description :: Ptr ConstantImpl -> IO CString

foreign import ccall safe "blpapi_constant.h blpapi_Constant_status"
    blpapi_Constant_status :: Ptr ConstantImpl -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_datatype"
    blpapi_Constant_datatype :: Ptr ConstantImpl -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsChar"
    blpapi_Constant_getValueAsChar :: Ptr ConstantImpl -> Ptr Char -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsInt32"
    blpapi_Constant_getValueAsInt32 :: Ptr ConstantImpl -> Ptr CInt -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsInt64"
    blpapi_Constant_getValueAsInt64 :: Ptr ConstantImpl -> Ptr CLLong -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsFloat32"
    blpapi_Constant_getValueAsFloat32 :: Ptr ConstantImpl
                                      -> Ptr CFloat
                                      -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsFloat64"
    blpapi_Constant_getValueAsFloat64 :: Ptr ConstantImpl
                                      -> Ptr CDouble
                                      -> IO Int

foreign import ccall safe
  "blpapi_constant.h blpapi_Constant_getValueAsDatetime"
    blpapi_Constant_getValueAsDatetime :: Ptr ConstantImpl
                                       -> Ptr BlpapiDatetime
                                       -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_Constant_getValueAsString"
    blpapi_Constant_getValueAsString :: Ptr ConstantImpl
                                     -> Ptr CString
                                     -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_name"
    blpapi_ConstantList_name :: Ptr ConstantListImpl -> IO (Ptr NameImpl)

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_description"
    blpapi_ConstantList_description :: Ptr ConstantListImpl -> IO CString

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_numConstants"
    blpapi_ConstantList_numConstants :: Ptr ConstantListImpl -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_datatype"
    blpapi_ConstantList_datatype :: Ptr ConstantListImpl -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_status"
    blpapi_ConstantList_status :: Ptr ConstantListImpl -> IO Int

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_getConstant"
    blpapi_ConstantList_getConstant :: Ptr ConstantListImpl
                                    -> CString
                                    -> Ptr NameImpl
                                    -> IO (Ptr ConstantImpl)

foreign import ccall safe "blpapi_constant.h blpapi_ConstantList_getConstantAt"
    blpapi_ConstantList_getConstantAt :: Ptr ConstantListImpl
                                      -> Int
                                      -> IO (Ptr ConstantImpl)


convertToStatus :: Int -> SchemaStatus
convertToStatus = toEnum

convertConstantValue :: Ptr ConstantImpl -> TI.BlpapiDataTypes -> IO T.Value
convertConstantValue ptr TI.BlpTypeChar =
    alloca
      (\v -> liftM T.BlpChar (blpapi_Constant_getValueAsChar ptr v >> peek v))
convertConstantValue ptr TI.BlpTypeInt32 =
    alloca (\v ->
      liftM (T.BlpInt32 . fromIntegral)
        (blpapi_Constant_getValueAsInt32 ptr v >> peek v))
convertConstantValue ptr TI.BlpTypeString =
    alloca (\v ->
      liftM (T.BlpString . Text.pack)
        (blpapi_Constant_getValueAsString ptr v >> peek v >>= peekCString))

convertConstantImplToConstant :: Ptr ConstantImpl -> IO Constant
convertConstantImplToConstant ptr = do
  nameImpl <- blpapi_Constant_name ptr
  des <- blpapi_Constant_description ptr >>= peekCString
  status <- blpapi_Constant_status ptr
  datatype <- blpapi_Constant_datatype ptr
  val <- convertConstantValue ptr (toEnum datatype)
  return $ Constant (nameImplToString nameImpl)
                    des
                    (convertToStatus status)
                    val

convertConstantListImplToConstants :: Ptr ConstantListImpl -> IO ConstantList
convertConstantListImplToConstants ptr = do
  nameImpl <- blpapi_ConstantList_name ptr
  desc <- blpapi_ConstantList_description ptr >>= peekCString
  status <- blpapi_ConstantList_status ptr
  cList <- func ptr
  return $ ConstantList (nameImplToString nameImpl)
                        desc
                        (convertToStatus status)
                        cList
    where
      func p = do
        num <- blpapi_ConstantList_numConstants p
        forM [0..(num-1)] $ \i -> do
                   constantPtr <- blpapi_ConstantList_getConstantAt ptr i
                   convertConstantImplToConstant constantPtr

convertSchemaTypeDefinitionImpl :: Ptr SchemaTypeDefinitionImpl
                                -> IO SchemaTypeDefinition
convertSchemaTypeDefinitionImpl ptr = do
  nameImpl <- blpapi_SchemaTypeDefinition_name ptr
  des <- blpapi_SchemaTypeDefinition_description ptr >>= peekCString
  status <- blpapi_SchemaTypeDefinition_status ptr
  datatype <- blpapi_SchemaTypeDefinition_datatype ptr
  typedef <- convertToTypeDef (toEnum datatype) ptr
  return $ SchemaTypeDefinition (nameImplToString nameImpl)
                                des
                                (convertToStatus status)
                                typedef

convertToTypeDef :: TI.BlpapiDataTypes
                 -> Ptr SchemaTypeDefinitionImpl
                 -> IO TypeDefinition
convertToTypeDef TI.BlpTypeBool _ = return  TypeDefinitionBool
convertToTypeDef TI.BlpTypeChar _ = return  TypeDefinitionChar
convertToTypeDef TI.BlpTypeByte _ = return  TypeDefinitionByte
convertToTypeDef TI.BlpTypeInt32 _ = return  TypeDefinitionInt32
convertToTypeDef TI.BlpTypeInt64 _ = return  TypeDefinitionInt64
convertToTypeDef TI.BlpTypeFloat32 _ = return  TypeDefinitionFloat32
convertToTypeDef TI.BlpTypeFloat64 _ = return  TypeDefinitionFloat64
convertToTypeDef TI.BlpTypeString _ = return  TypeDefinitionString
convertToTypeDef TI.BlpTypeByteArray _ = return TypeDefinitionByteArray
convertToTypeDef TI.BlpTypeDate _ = return TypeDefinitionDate
convertToTypeDef TI.BlpTypeTime _ = return TypeDefinitionTime
convertToTypeDef TI.BlpTypeDecimal _ = undefined
convertToTypeDef TI.BlpTypeDatetime _ = return TypeDefinitionDateTime
convertToTypeDef TI.BlpTypeEnum ptr = do
  en <- blpapi_SchemaTypeDefinition_enumeration ptr
  cl <- convertConstantListImplToConstants en
  return $ TypeDefinitionEnum cl

convertToTypeDef TI.BlpTypeSeq ptr = do
  num <- blpapi_SchemaTypeDefinition_numElementDefinitions ptr
  lst <- forM [0..(num-1)] (
              blpapi_SchemaTypeDefinition_getElementDefinitionAt ptr
                  >=> convertSchemaDefinitionImpl)
  return $ TypeDefinitionSequence lst

convertToTypeDef TI.BlpTypeChoice ptr = do
  num <- blpapi_SchemaTypeDefinition_numElementDefinitions ptr
  lst <- forM [0..(num-1)] $
                    blpapi_SchemaTypeDefinition_getElementDefinitionAt ptr
                      >=> convertSchemaDefinitionImpl
  return $ TypeDefinitionChoice lst


convertSchemaDefinitionImpl :: Ptr SchemaDefinitionImpl -> IO SchemaDefinition
convertSchemaDefinitionImpl ptr = do
  nameImpl <- blpapi_SchemaElementDefinition_name ptr
  des <- blpapi_SchemaElementDefinition_description ptr >>= peekCString
  status <- blpapi_SchemaElementDefinition_status ptr
  typePtr <- blpapi_SchemaElementDefinition_type ptr
  schemaType <- convertSchemaTypeDefinitionImpl typePtr
  alternateName <- getAlternateName ptr
  minV <- blpapi_SchemaElementDefinition_minValues ptr
  maxV <- blpapi_SchemaElementDefinition_maxValues ptr
  return $ SchemaDefinition
             (nameImplToString nameImpl)
             des
             (convertToStatus status)
             alternateName
             minV
             maxV
             schemaType
      where
        getAlternateName sPtr = do
          num <- blpapi_SchemaElementDefinition_numAlternateNames sPtr
          forM [0..(num-1)] $
              blpapi_SchemaElementDefinition_getAlternateName sPtr
                >=> return . nameImplToString

