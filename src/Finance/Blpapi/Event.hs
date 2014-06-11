{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Finance.Blpapi.Event
Description : An event resulting for subscription or request
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}

module Finance.Blpapi.Event where

import           Finance.Blpapi.Service

import           Control.Monad
import           Data.Char
import           Data.Map                     (Map)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Finance.Blpapi.CorrelationId
import           GHC.Float

-- | A single event resulting from a subscription or a request.
--
-- Events are created by the 'API' and passed to the application through the
-- 'SessionHandler'. The event is the basic unit of work provided to
-- applications. Each Event consists of an 'EventType' and a 'Message'.
data Event =
  Event { -- | The type of 'Message's contained in this event
          eventType    :: !EventType,
          -- \ The event content
          eventContent :: !Message
} deriving (Show, Eq)

-- | The possible types of events
data EventType
      -- | Undefined event type (Should never happen)
      = EventTypeUndefined
      -- | Admin Event
      | EventTypeAdmin
      -- | Status updates for Session
      | EventTypeSessionStatus
      -- | Status updates for Subscription
      | EventTypeSubscriptionStatus
      -- | Status update for Request
      | EventTypeRequestStatus
      -- | The final (and possibly only) response to a request
      | EventTypeResponse
      -- | A partial response to a request
      | EventTypePartialResponse
      -- | Unknown Event Type (Should never happen)
      | EventTypeUnknown
      -- | Data updates resulting from subscription
      | EventTypeSubscriptionData
      -- | Status updates for a service
      | EventTypeServiceStatus
      -- | A timeout event (Should never happen)
      | EventTypeTimeout
      -- | Status updates for user authorization
      | EventTypeAuthorizationStatus
      -- | Status updates for a resolution operation
      | EventTypeResolutionStatus
      -- | Status updates about topics for service providers
      | EventTypeTopicStatus
      -- | Status updates for a generate token request
      | EventTypeTokenStatus
      -- | A request event to respond to
      | EventTypeRequest
                 deriving (Show, Eq, Enum, Bounded)

-- | Message is the actual data corresponding to an 'Event'. It is
-- associated with a 'Service' and one or more 'CorrelationId's. The
-- message contents are represented as 'Element'. Each message also contains
-- a fragment type, which informs whether the message is a part of a bigger
-- chunk.
data Message =
  Message { -- | The topic name associated with the message.
            messageTopicName      :: !Text,
            -- | The service which the message is from
            messageService        :: Maybe Service,
            -- | The message content
            messageData           :: !Element,
            -- | The message fragment type
            messageFragmentType   :: !MessageFragmentType,
            -- | The list of 'CorrelationId's
            messageCorrelationIds :: ![CorrelationId]
    } deriving (Eq)

-- | A message could be split into more than one fragments to reduce
-- each message size. This enumeration is used to indicate whether a message
-- is a fragmented message and the position in the fragmented messages.
data MessageFragmentType
      -- | Message is not fragmented
      = MessageFragmentNone
      -- | The first fragmented message
      | MessageFragmentStart
      -- | Intermediate fragmented messages
      | MessageFragmentIntermediate
      -- | The last fragmented message
      | MessageFragmentEnd
           deriving (Show, Eq, Enum, Bounded)

-- | An array of Elements
type ElementArray = [Element]

-- | A type determining an 'Element' object. Its a map between the element
-- name and its content along with the contents schema definition.
type ElementObject = Map Text ElementWithDefinition

-- | The message content type.
-- An Element can represent: a single value of any data type supported by
-- the Bloomberg API; an array of values; a sequence or a choice.
data Element = ElementBool {elementBoolValue :: !Bool}
             | ElementChar {elementCharValue :: !Char}
             | ElementInt  {elementIntValue :: !Int}
             | ElementInt64 {elementIntegerValue :: !Integer}
             | ElementFloat {elementFloatValue :: !Float}
             | ElementDouble {elementDoubleValue :: !Double}
             | ElementString {elementStringValue :: !Text}
             | ElementDate {elementDateValue    :: !Day,
                            elementDateTimeZone :: Maybe TimeZone}
             | ElementTime {elementTimeValue    :: !TimeOfDay,
                            elementTimeTimeZone :: Maybe TimeZone}
             | ElementDatetime {elementDatetimeValue    :: !LocalTime,
                                elementDatetimeTimeZone :: Maybe TimeZone}
             | ElementEnum {elementEnumValue :: !String}
             | ElementSequence {elementSequenceValue :: !ElementObject}
             | ElementChoice {elementChoiceValue :: !ElementObject}
             | ElementArray {elementArrayValue:: !ElementArray }
             | ElementNull
    deriving (Show, Eq)

-- | An element along with its schema definition
data ElementWithDefinition = ElementWithDefinition {
      elementWithDefinitionSchema  :: !SchemaDefinition,
      elementWithDefinitionContent :: !Element
} deriving (Eq)

instance Show Message where
    show (Message topic _ e ft cl)
          = "TopicName: " ++ show topic
          ++ ", " ++ show e
          ++ ", MessageFragmentType: " ++ show ft
          ++ ", Correlations: " ++ show cl

instance Show ElementWithDefinition where
    show (ElementWithDefinition _ t)
      = "ElementType: { " ++ show t  ++ " }"

-- | A class which determines the types that an Element can be
-- converted to
class BlpConversionUtil r where
    blpConvert :: Element -> Maybe r

instance BlpConversionUtil Bool where
    blpConvert (ElementBool a) = Just a
    blpConvert _ = Nothing

instance BlpConversionUtil Char where
    blpConvert (ElementBool a) | a = Just 'T'
                               | otherwise = Just 'F'
    blpConvert (ElementChar c) = Just c
    blpConvert _ = Nothing

instance BlpConversionUtil Int where
    blpConvert (ElementChar c) = Just (ord c)
    blpConvert (ElementInt c) = Just c
    blpConvert (ElementFloat c)
        | floor c > (minBound :: Int) && floor c < (maxBound :: Int)
            = Just $ floor c
        | otherwise = Nothing
    blpConvert (ElementDouble c)
        | floor c > (minBound :: Int) && floor c < (maxBound :: Int)
            = Just $ floor c
        | otherwise = Nothing
    blpConvert _ = Nothing

instance BlpConversionUtil Integer where
    blpConvert (ElementChar c) = Just $ (toInteger . ord) c
    blpConvert (ElementInt c) = Just $ toInteger c
    blpConvert (ElementInt64 c) = Just c
    blpConvert (ElementFloat c) = Just $ floor c
    blpConvert (ElementDouble c) = Just $ floor c
    blpConvert _ = Nothing

instance BlpConversionUtil Float where
    blpConvert (ElementChar c) = Just $ (fromIntegral . ord) c
    blpConvert (ElementInt c) = Just $ fromIntegral c
    blpConvert (ElementFloat c) = Just c
    blpConvert _ = Nothing

instance BlpConversionUtil Double where
    blpConvert (ElementChar c) = Just $ (fromIntegral . ord) c
    blpConvert (ElementInt c) = Just $ fromIntegral c
    blpConvert (ElementFloat c) = Just $ float2Double c
    blpConvert (ElementDouble c) = Just c
    blpConvert _ = Nothing


instance BlpConversionUtil TimeOfDay where
    blpConvert (ElementTime c _) = Just c
    blpConvert (ElementDatetime c _) = Just $ localTimeOfDay c
    blpConvert _ = Nothing

instance BlpConversionUtil Day where
    blpConvert (ElementDate c _) = Just c
    blpConvert (ElementDatetime c _) = Just $ localDay c
    blpConvert _ = Nothing

instance BlpConversionUtil ZonedTime where
    blpConvert (ElementDatetime c (Just z)) = Just $ ZonedTime c z
    blpConvert _ = Nothing

instance BlpConversionUtil LocalTime where
    blpConvert (ElementDatetime c _) = Just c
    blpConvert _ = Nothing

instance BlpConversionUtil String where
    blpConvert (ElementBool c) = Just $ show c
    blpConvert (ElementChar c) = Just $ show c
    blpConvert (ElementInt c) = Just $ show c
    blpConvert (ElementInt64 c) = Just $ show c
    blpConvert (ElementFloat c) = Just $ show c
    blpConvert (ElementDouble c) = Just $ show c
    blpConvert (ElementString c) = Just $ show c
    blpConvert (ElementDate c _) = Just $ show c
    blpConvert (ElementTime c _) = Just $ show c
    blpConvert (ElementDatetime c (Just z)) = Just $ show $ ZonedTime c z
    blpConvert (ElementDatetime c Nothing) = Just $ show c
    blpConvert (ElementNull) = Just ""

instance BlpConversionUtil Text where
    blpConvert (ElementString c) = Just c
    blpConvert v = liftM T.pack $ blpConvert v

