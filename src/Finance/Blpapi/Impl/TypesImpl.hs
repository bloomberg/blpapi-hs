{-|
Module      : Finance.Blpapi.Impl.TypesImpl
Description : FFI for Types
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.TypesImpl where

data BlpapiDataTypes =
    BlpUndefined |
    BlpTypeBool |
    BlpTypeChar |
    BlpTypeByte |
    BlpTypeInt32 |
    BlpTypeInt64 |
    BlpTypeFloat32 |
    BlpTypeFloat64 |
    BlpTypeString |
    BlpTypeByteArray |
    BlpTypeDate |
    BlpTypeTime |
    BlpTypeDecimal |
    BlpTypeDatetime |
    BlpTypeEnum |
    BlpTypeSeq |
    BlpTypeChoice
    deriving (Enum, Show, Bounded, Eq)


