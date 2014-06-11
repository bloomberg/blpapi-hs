{-|
Module      : Finance.Blpapi.Service
Description : Schema Types for request/response and subscriptions
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

All API data is associated with a 'Service'. A service object is obtained
from a Session and contains zero or more 'Operations'. A service can be a
provider service (can generate API data) or a consumer service.
-}
module Finance.Blpapi.Service where

import qualified Finance.Blpapi.Types as T

-- | This data type defines a service which provides access to API data.
--
-- A 'Service' is obtained from a 'Session' and contains the 'Operation's
-- (each of which contains its own 'SchemaDefinition') and the
-- 'SchemaDefinition' for Events which this 'Service' may produce.
--
-- All API data is associated with a service. Before accessing API data
-- using either request-reply or subscription, the appropriate 'Service'
-- must be 'opened' and, if necessary, authorized.
--
-- Once a Service has been successfully opened in a Session it remains
-- accessible until the Session is terminated.
data Service =
  Service { -- | The service name
            serviceName                     :: !String,
            -- | The service description
            serviceDescription              :: !String,
            -- | The service name used to authorize this service
            serviceAuthorizationServiceName :: !String,
            -- | The list of supported operations
            serviceOperations               :: ![Operation],
            -- | The schema which defines the 'Event's which will be
            -- delivered when subscription is successful
            serviceEventDefinitions         :: ![SchemaDefinition]
          } deriving (Show, Eq, Ord)

-- | This data type represents an operation that can be performed on
-- a service
data Operation =
  Operation { -- | The name of the operation
              operationName           :: !String,
              -- | The description of the operation
              operationDescription    :: !String,
              -- | The schema which determines the request structure
              operationRequestSchema  :: !SchemaDefinition,
              -- | The schema for this list of responses that are tied to
              -- this operation
              operationResponseSchema :: ![SchemaDefinition]
            } deriving (Show, Eq, Ord)


-- | This data type represents the definition of an individual field within a
-- schema type. An element is defined by an identifier/name, a type, and the
-- number of values of that type that may be associated with the
-- identifier/name. In addition, this class offers access to meta data
-- providing a description and deprecation status for the field.
--
-- An optional element has 'schemaDefMinValues == 0'.
--
-- A mandatory element has 'schemaDefMinValues >= 1'.
--
-- An element that must contain a single value has
-- 'schemaDefMinValues == schemaDefMaxValues == 1'.
--
-- An element containing an array has 'schemaDefMaxValues > 1'.
data SchemaDefinition =
  SchemaDefinition {  -- | The nsame of schema definition
                      schemaDefName           :: !String,
                      -- | The description
                      schemaDefDescription    :: !String,
                      -- | The schema status
                      schemaDefStatus         :: !SchemaStatus,
                      -- | The list of alternate names
                      schemaDefAlternateNames :: ![String],
                      -- | The minimun values of occurences of this Element
                      schemaDefMinValues      :: !Int,
                      -- | The maximum values of occurences of this Element
                      schemaDefMaxValues      :: !Int,
                      -- | The definition of the type
                      schemaDefTypeDefinition :: !SchemaTypeDefinition
                   } deriving (Show, Eq, Ord)

-- This data type in addition to the type's structure also provides the
-- metadata containing a description and deprecation status
data SchemaTypeDefinition = SchemaTypeDefinition {
      schemaTypeName        :: String,
      scehmaTypeDescription :: String,
      schemaTypeStatus      :: SchemaStatus,
      schemaTypeDefinition  :: TypeDefinition

} deriving (Show, Eq, Ord)

-- | Enumeration of the possible deprection statuses of the schema element
-- or type
data SchemaStatus
  -- | This item is current and may appear in Messages
  = SchemaActive
  -- | This item is current and may appear in Messages but will be removed
  -- in due course
  | SchemaDeprecated
  -- | This item is not current and will not appear in Messages
  | SchemaInactive
  -- | This item is expected to be deprecated in the future; clients are
  -- advised to migrate away from use of this item.
  | SchemaPendingDeprecation
    deriving (Show, Eq, Enum, Ord)

-- |Represents the value of a schema enumeration constant.
data Constant =
  Constant { -- | The symbolic name of the Constant
             constantName       :: !String,
             -- | The description of the Constant
             constantDescripton :: !String,
             -- | The status of the schema
             constantStatus     :: !SchemaStatus,
             -- | The underlying value of the Constant
             constantValue      :: !T.Value
           } deriving (Show, Eq, Ord)

-- | Represents a list schema enumeration constant
data ConstantList =
  ConstantList {  -- | The 'ConstantList' name
                  constantListName        :: !String,
                  -- | The description
                  constantListDescription :: !String,
                  -- | The status defined by the schema
                  constantListStatus      :: !SchemaStatus,
                  -- | The list of Constant
                  constantListValues      :: ![Constant]
               } deriving (Show, Eq, Ord)

-- | This data type is a representation of a "type" that can be used
-- within a schema, including both simple atomic types (integers, dates,
-- strings, etc.) as well as "complex" types defined a sequences of or
-- choice among a collection (named) elements, each of which is in turn
-- described by another type.
data TypeDefinition
    = TypeDefinitionSequence {
        sequenceDefinition :: [SchemaDefinition]
      }
    | TypeDefinitionChoice {
        choiceDefinition :: [SchemaDefinition]
      }
    | TypeDefinitionBool
    | TypeDefinitionChar
    | TypeDefinitionByte
    | TypeDefinitionInt32
    | TypeDefinitionInt64
    | TypeDefinitionFloat32
    | TypeDefinitionFloat64
    | TypeDefinitionString
    | TypeDefinitionByteArray
    | TypeDefinitionDate
    | TypeDefinitionTime
    | TypeDefinitionDateTime
    | TypeDefinitionEnum {
        constantList :: ConstantList
      }
    deriving (Show, Eq, Ord)


