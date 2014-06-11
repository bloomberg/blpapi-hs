{-|
Module      : Finance.Blpapi.CorrelationId
Description : Id that can be associated with Requests or Subscriptions
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.CorrelationId where

import           Data.Int

data CorrelationIdType = UserGeneratedCid | InternallyGeneratedCid
    deriving (Show, Eq, Ord)

data CorrelationId = CorrelationId {
    correlationType  :: CorrelationIdType,
    correlationClass :: Int,
    correlationValue :: Int64
} deriving (Show, Eq, Ord)

-- | Create 'CorrelationId' by providing an integer value and
-- a 'ClassId.' The 'ClassId' is an easier way to group 'CorrelationId'.
-- Please note that 'ClassId' [0,100] are reserved by the Haskell Bindings.
createCorrelationIdWithClassId :: Int -> Int -> CorrelationId
createCorrelationIdWithClassId c v =
    CorrelationId UserGeneratedCid c (fromIntegral v)

-- | Create 'CorrelationId' without any class information. The 'CorrelationId'
-- can then be used with any subscription or request. Any updates or responses
-- will contain the same 'CorrelationId' that was used (if used).
createCorrelationId :: Int -> CorrelationId
createCorrelationId v = CorrelationId UserGeneratedCid 0 (fromIntegral v)
