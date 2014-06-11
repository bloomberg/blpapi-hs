{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : Finance.Blpapi.Impl.DatetimeImpl
Description : FFI for Datetime
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.DatetimeImpl where

import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import           Data.Time.Calendar
import           Data.Time.LocalTime

type DatetimeImpl = (Ptr BlpapiDatetime)
type HPDatetimeImpl = (Ptr BlpapiHPDatetime)

kBLPAPIDATETIMEYEARPART :: Int8
kBLPAPIDATETIMEYEARPART =         0x1 :: Int8
kBLPAPIDATETIMEMONTHPART :: Int8
kBLPAPIDATETIMEMONTHPART =        0x2 :: Int8
kBLPAPIDATETIMEDAYPART :: Int8
kBLPAPIDATETIMEDAYPART =          0x4 :: Int8
kBLPAPIDATETIMEOFFSETPART :: Int8
kBLPAPIDATETIMEOFFSETPART =       0x8 :: Int8
kBLPAPIDATETIMEHOURSPART :: Int8
kBLPAPIDATETIMEHOURSPART =        0x10 :: Int8
kBLPAPIDATETIMEMINUTESPART :: Int8
kBLPAPIDATETIMEMINUTESPART =      0x20 :: Int8
kBLPAPIDATETIMESECONDSPART :: Int8
kBLPAPIDATETIMESECONDSPART =      0x40 :: Int8
kBLPAPIDATETIMEFRACSECONDSPART :: Int8
kBLPAPIDATETIMEFRACSECONDSPART =  0x80 :: Int8
kBLPAPIDATETIMEDATEPART :: Int8
kBLPAPIDATETIMEDATEPART
  =  kBLPAPIDATETIMEDAYPART
  .|. kBLPAPIDATETIMEMONTHPART
  .|. kBLPAPIDATETIMEYEARPART :: Int8
kBLPAPIDATETIMETIMEPART :: Int8
kBLPAPIDATETIMETIMEPART
 =  kBLPAPIDATETIMEHOURSPART
 .|. kBLPAPIDATETIMEMINUTESPART
 .|. kBLPAPIDATETIMESECONDSPART :: Int8
kBLPAPIDATETIMEDATETIMEPART :: Int8
kBLPAPIDATETIMEDATETIMEPART
 =  kBLPAPIDATETIMEDATEPART .|. kBLPAPIDATETIMETIMEPART :: Int8

data BlpapiDatetime = BlpapiDatetime
    { bdtParts        :: !Int8,
      bdtHours        :: !Int8,
      bdtMinutes      :: !Int8,
      bdtSeconds      :: !Int8,
      bdtMilliseconds :: !Int16,
      bdtMonth        :: !Int8,
      bdtDay          :: !Int8,
      bdtYear         :: !Int16,
      bdtOffset       :: !Int16
} deriving (Show, Eq)

data BlpapiHPDatetime = BlpapiHPDatetime {
      hpDatetime    :: BlpapiDatetime,
      hpPicoSeconds :: Int32
} deriving (Show, Eq)

instance Storable BlpapiDatetime where
    sizeOf    _ = 12
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        parts <- peekByteOff ptr 0
        hours <- peekByteOff ptr 1
        minutes <- peekByteOff ptr 2
        seconds <- peekByteOff ptr 3
        milliSeconds <- peekByteOff ptr 4
        month <- peekByteOff ptr 6
        day <- peekByteOff ptr 7
        year <- peekByteOff ptr 8
        offset <- peekByteOff ptr 10
        return (BlpapiDatetime parts
                               hours
                               minutes
                               seconds
                               milliSeconds
                               month
                               day
                               year
                               offset)

    poke ptr (BlpapiDatetime parts
                             hours
                             minutes
                             seconds
                             milliSeconds
                             month
                             day
                             year
                             offset) =
            do
              pokeByteOff ptr 0 parts
              pokeByteOff ptr 1 hours
              pokeByteOff ptr 2 minutes
              pokeByteOff ptr 3 seconds
              pokeByteOff ptr 4 milliSeconds
              pokeByteOff ptr 6 month
              pokeByteOff ptr 7 day
              pokeByteOff ptr 8 year
              pokeByteOff ptr 10 offset

instance Storable BlpapiHPDatetime where
    sizeOf    _ = 16
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        datetime <- peekByteOff ptr 0
        picoseconds <- peekByteOff ptr 12
        return (BlpapiHPDatetime datetime picoseconds)

    poke ptr (BlpapiHPDatetime datetime picoseconds) =
            do
              pokeByteOff ptr 0 datetime
              pokeByteOff ptr 12 picoseconds


blpapiDateTimeWriter :: CString -> CInt -> Ptr CInt -> IO Int
blpapiDateTimeWriter str _ _ = do
  s <- peekCString str
  putStrLn s
  return 0

foreign import ccall "blpapi_datetime.h blpapi_Datetime_print"
    blpapi_Datetime_print :: DatetimeImpl
                          -> FunPtr ( CString -> CInt -> Ptr CInt -> IO Int)
                          -> Ptr CInt
                          -> Int
                          -> Int
                          -> IO Int

foreign import ccall "blpapi_datetime.h blpapi_HighPrecisionDatetime_print"
    blpapi_HighPrecisionDatetime_print
       :: HPDatetimeImpl
       -> FunPtr ( CString -> CInt -> Ptr CInt -> IO Int)
       -> Ptr CInt
       -> Int
       -> Int
       -> IO Int

getParts :: BlpapiHPDatetime -> Int8
getParts = bdtParts . hpDatetime

getYear :: BlpapiDatetime -> Integer
getYear d | bdtParts d .&. kBLPAPIDATETIMEYEARPART == kBLPAPIDATETIMEYEARPART
    = fromIntegral $ bdtYear d

getMonth :: BlpapiDatetime -> Int
getMonth
  d | bdtParts d .&. kBLPAPIDATETIMEMONTHPART == kBLPAPIDATETIMEMONTHPART
    = fromIntegral $ bdtMonth d

getDay :: BlpapiDatetime -> Int
getDay d | bdtParts d .&. kBLPAPIDATETIMEDAYPART == kBLPAPIDATETIMEDAYPART
    = fromIntegral $ bdtDay d

getHours :: BlpapiDatetime -> Int
getHours
  d | bdtParts d .&. kBLPAPIDATETIMEHOURSPART == kBLPAPIDATETIMEHOURSPART
    = fromIntegral $ bdtHours d

getMinutes :: BlpapiDatetime -> Int
getMinutes
  d | bdtParts d .&. kBLPAPIDATETIMEMINUTESPART == kBLPAPIDATETIMEMINUTESPART
    = fromIntegral $ bdtMinutes d

getSeconds :: BlpapiDatetime -> Integer
getSeconds
  d | bdtParts d .&. kBLPAPIDATETIMESECONDSPART == kBLPAPIDATETIMESECONDSPART
    = fromIntegral $ bdtSeconds d

getFracSeconds :: BlpapiHPDatetime -> Integer
getFracSeconds d
  | (bdtParts . hpDatetime) d .&. kBLPAPIDATETIMEFRACSECONDSPART
      == kBLPAPIDATETIMEFRACSECONDSPART
    = fromIntegral $ hpPicoSeconds d
  | otherwise = 0

totalSeconds :: BlpapiHPDatetime -> Rational
totalSeconds d
  = toRational (getFracSeconds d +
                  (getSeconds . hpDatetime) d * (10^ (12::Int)))
               / (10^(12 :: Int):: Rational)

data BlpDateTimeImpl
  = BlpDateImpl !Day
  | BlpTimeImpl !TimeOfDay
  | BlpLocalTimeImpl !LocalTime
  | BlpBadValue

toDay :: BlpapiHPDatetime -> BlpDateTimeImpl
toDay d' = BlpDateImpl $ fromGregorian (getYear d) (getMonth d) (getDay d)
    where d = hpDatetime d'

toTime :: BlpapiHPDatetime -> BlpDateTimeImpl
toTime d'
  = BlpTimeImpl
      (TimeOfDay (getHours d) (getMinutes d) (realToFrac (totalSeconds d')))
    where d = hpDatetime d'

toDateTime :: BlpapiHPDatetime -> BlpDateTimeImpl
toDateTime d = BlpLocalTimeImpl $ LocalTime day' time'
    where
        BlpDateImpl day' = toDay d
        BlpTimeImpl time' = toTime d

toTimeZone :: BlpapiHPDatetime -> Maybe TimeZone
toTimeZone d = if getParts d .&. kBLPAPIDATETIMEOFFSETPART == kBLPAPIDATETIMEOFFSETPART
               then Just $ minutesToTimeZone $ (fromIntegral.bdtOffset.hpDatetime) d
               else Nothing

convertBlpapiHPDatimeImpl :: BlpapiHPDatetime
                          -> (BlpDateTimeImpl, Maybe TimeZone)
convertBlpapiHPDatimeImpl d
 | getParts d .&. kBLPAPIDATETIMEDATETIMEPART == kBLPAPIDATETIMEDATETIMEPART
    = (toDateTime d, toTimeZone d)
 | getParts d .&. kBLPAPIDATETIMEDATEPART == kBLPAPIDATETIMEDATEPART
    = (toDay d, toTimeZone d)
 | getParts d .&. kBLPAPIDATETIMETIMEPART == kBLPAPIDATETIMETIMEPART
    = (toTime d, toTimeZone d)
 | otherwise = (BlpBadValue, Nothing)

