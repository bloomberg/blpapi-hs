{-|
Module      : Finance.Blpapi.SessionOptions
Description : User specified options for creating a 'Session'
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

This module contains the options that the user can specify when creating
a 'Session'.

To reduce the length of record accessors, the record name has *not* been
pre-pended to them. It is advisable to use a qualified import for this
module.
-}
module Finance.Blpapi.SessionOptions where

-- | Type representing an API end point
data ServerAddress =
  ServerAddress { -- | The url for the API end point
                  serverAddressHost :: String,
                  -- | The port for the API end point
                  serverAddressPort :: Int
                } deriving (Show, Eq)

-- | The possible options for connecting to an API end point
data ClientMode
        -- | Automatic i.e. Desktop if available, otherwise server
        = AUTO
        -- | Always connect to the Desktop API
        | DAPI
        -- | Always connect to the Server API
        | SAPI
          deriving (Show, Eq, Enum)

-- | All the options that can be configured for a 'Session'
data SessionOptions = SessionOptions {
      serverAddresses                :: [ServerAddress],
      connectTimeoutMs               :: Int,
      defaultService                 :: String,
      defaultSubscriptionService     :: String,
      defaultTopicPrefix             :: String,
      allowMultipleCorrelatorsPerMsg :: Bool,
      clientMode                     :: ClientMode,
      maxPendingRequests             :: Int,
      autoRestartOnDisconnection     :: Bool,
      numStartAttempts               :: Int,
      authenticationOptions          :: String,
      maxEventQueueSize              :: Int,
      slowConsumerWarningLoWaterMark :: Float,
      slowConsumerWarningHiWaterMark :: Float,
      keepAliveInactivityTimeSec     :: Int,
      keepAliveResponseTimeoutSec    :: Int
} deriving (Show, Eq)

defaultSessionOptions :: SessionOptions
defaultSessionOptions = SessionOptions {
      serverAddresses = [ServerAddress "localhost" 8194],
      connectTimeoutMs = 5000,
      defaultService = "",
      defaultSubscriptionService = "//blp/mktdata",
      defaultTopicPrefix = "ticker",
      allowMultipleCorrelatorsPerMsg = False,
      clientMode = AUTO,
      maxPendingRequests = 1024,
      autoRestartOnDisconnection = False,
      numStartAttempts = 1,
      authenticationOptions = "",
      maxEventQueueSize = 1000,
      slowConsumerWarningLoWaterMark = 0.5,
      slowConsumerWarningHiWaterMark = 0.75,
      keepAliveInactivityTimeSec = 20,
      keepAliveResponseTimeoutSec = 5
 }
