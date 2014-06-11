{-|
Module      : Finance.Blpapi.Identity
Description : User Identity for requests and subscriptions
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Identity  where

import           Finance.Blpapi.Impl.IdentityImpl

-- | The identity that will be used to send requests and start
-- subscriptions.
newtype Identity = Identity {
                      identityImpl :: IdentityHandle
                   } deriving (Show, Eq)
