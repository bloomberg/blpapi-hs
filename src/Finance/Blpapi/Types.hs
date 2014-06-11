{-|
Module      : Finance.Blpapi.Types
Description : Blpapi value types
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

Value types for BLPAPI
-}
module Finance.Blpapi.Types where

import           Data.Int
import           Data.Text
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Data.Word

-- | The various value types
data Value = BlpString !Text
           | BlpBool !Bool
           | BlpInt32 !Int
           | BlpInt64 !Int64
           | BlpChar !Char
           | BlpFloat !Float
           | BlpDouble !Double
           | BlpByte !Word8
           | BlpDateTime {
                blpLocalTime     :: !LocalTime,
                blpLocalTimeZone :: Maybe TimeZone
             }
           | BlpDate {
                blpDay         :: !Day,
                blpDayTimeZone :: Maybe TimeZone
             }
           | BlpTime {
                blpTimeOfDay :: !TimeOfDay,
                blpTimeZone  :: Maybe TimeZone
             }
             deriving (Show, Eq, Ord)

