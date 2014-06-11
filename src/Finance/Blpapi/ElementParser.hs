{-|
Module      : Finance.Blpapi.ElementParser
Description : Parsing Utility for 'Element'
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

Utility functions to help parse an 'Element' which is received from an 'Event'.

    @
    parseRef :: Element -> Either String [RefDataResponse]
    parseRef el = do
        e <- P.getElement "ReferenceDataResponse" el
        secDatas <- P.getElement "securityData" e >>= P.getArrayElements
        foldM (\acc it -> do
           secName <- P.getElement "security" it >>= P.getValue
           fieldDatas <- P.getElement "fieldData" it >>= P.getAllElements
           fieldValues <- foldM (\ acc2 (fn, it2) -> do
              value <- P.getValue it2
              Right ((fn, value):acc2)) [] fieldDatas
           return ((secName fieldValues):acc)) [] secDatas
    @

Some terminology: A complex type is 'ElementSequence' or 'ElementChoice'
according to 'SchemaDefinition' A Simple Type is a type in the
'SchemaDefinition' which is neither an array nor a Complex type like
'ElementSequence' or 'ElementChoice'
-}

module Finance.Blpapi.ElementParser where

import           Control.Arrow
import           Control.Monad.State
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           Finance.Blpapi.Event

-- | Return the 'Element' for the specified name in the specified element.
-- If there does not exist any sub-element with the specified name, the
-- return Left <error-string>
getElement :: T.Text -> Element -> Either String Element
getElement n (ElementSequence seqs)
    = case Map.lookup n seqs of
        Just a -> Right $ elementWithDefinitionContent a
        _ -> Left $ "Failed to find subElement: " ++ show n
getElement n (ElementChoice seqs)
    = case Map.lookup n seqs of
        Just a -> Right $ elementWithDefinitionContent a
        _ -> Left $ "Failed to find subElement: " ++ show n
getElement _ _
    = Left "Element is not a sequence or choice"

-- | Return all the sub-elements for the current 'Element'. If the current
-- element is not a 'ComplexType' then return Left <error-string>
getAllElements :: Element -> Either String [(T.Text, Element)]
getAllElements (ElementSequence seqs)
    = Right $ map (second elementWithDefinitionContent) $ Map.toList seqs
getAllElements (ElementChoice seqs)
    = Right $ map (second elementWithDefinitionContent) $ Map.toList seqs
getAllElements e
    = Left $ "Not an Element to get all the Elements: " ++ show e

-- | Return the value in the current 'Element'. If the current element
-- is not a 'SimpleType' then return Left <error-string>
getValue :: (BlpConversionUtil a) => Element -> Either String a
getValue et = case blpConvert et of
    Just v -> Right v
    Nothing -> Left $ "Failed to convert Element: " ++ show et

-- | Return the list of values in the specified 'Element'. If the current
-- element is not an array of simple types then return Left <Error-String>
getArrayValues :: (BlpConversionUtil a) => Element -> Either String [a]
getArrayValues (ElementArray seqs)
    = foldM (\ acc v -> case blpConvert v of
                Just val -> Right (val:acc)
                Nothing -> Left "Failed to Convert one of the values") [] seqs
getArrayValues et
    = Left $ "Element: " ++ show et ++ "not is an Array of value types"

-- | Return the list of 'Element's in the specified 'Element'. If the
-- current element is not a array of complex types then return Left
-- <error-string>
getArrayElements :: Element -> Either String [Element]
getArrayElements (ElementArray seqs@(ElementSequence _:_))
  = Right seqs
getArrayElements (ElementArray seqs@[ElementChoice _])
  = Right seqs
getArrayElements et
  = Left $ "Element: " ++ show et ++ "is not an Array of Elements"

