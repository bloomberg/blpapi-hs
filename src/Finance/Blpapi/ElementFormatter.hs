{-|
Module      : Finance.Blpapi.ElementFormatter
Description : Format Requests
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

This module provides an easy (Monadic) way for generating a Request (Element).
The formatting needs to satisfy the Schema, if it does not an Exception will
be thrown.

All the operations are performed inside the ElementFormatter, which is
a Monad transformer over the 'Blpapi' monad. Hence all the impure
operations like IO can be performed.

Some terminology: A complex type is 'ElementSequence' or 'ElementChoice'
according to 'SchemaDefinition' A Simple Type is a type in the
'SchemaDefinition' which is neither an array nor a Complex type like
'ElementSequence' or 'ElementChoice'

      @
      req <- createRequest ser "ReferenceDataRequest"
      formatRequest req $! do
          formatSubElement "returnEids" $
              setValue (BT.BlpBool True)
          formatSubElement "fields" $ do
              appendValue (BT.BlpString "ASK")
              appendValue (BT.BlpString "BID")
          formatSubElement "securities" $ do
              appendValue (BT.BlpString "IBM US Equity")
              appendValue (BT.BlpString "GOOG US Equity")
      @
-}

module Finance.Blpapi.ElementFormatter (
  ElementFormatter,

  setValue,
  setValueAt,

  appendValue,
  appendElement,

  formatSubElement,
  formatRequest
  ) where

import           Finance.Blpapi.Impl.ElementImpl
import           Finance.Blpapi.Session

import           Control.Applicative
import           Control.Monad.State.Strict
import           Finance.Blpapi.Impl.ErrorImpl

import           Finance.Blpapi.Impl.RequestImpl
import           Finance.Blpapi.Request

import qualified Finance.Blpapi.Types            as T

import qualified Data.Text                       as Text
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

-- | The State for the ElementFormatter 'StateT'
data ElementFormatterState = ElementFormatterState {
    _elementWrittable :: !ElementWritable
    }

-- | The State transformer over 'Blpapi' that is used to generate a request
-- that is compatible with the schema
newtype ElementFormatter a = ElementFormatter {
      getFormatter :: StateT ElementFormatterState Blpapi a
    }

-- | Strong type that represents an 'Element' that is only writable
newtype ElementWritable = ElementWritable {
    getElementImpl :: Ptr ElementImpl
}

instance Functor ElementFormatter where
    fmap = liftM

instance Applicative ElementFormatter where
    pure = formatterReturn
    (<*>) = ap
    

-- | The monad instance
instance Monad ElementFormatter where
    (>>=) = formatterBind
    return = formatterReturn
    --fail = formatterFail -- Lets Throw right now

-- | The MonadBlpapi instance
instance MonadBlpapi ElementFormatter where
    liftBlpapi m = ElementFormatter $! liftBlpapi m

formatterBind :: ElementFormatter a
              -> (a -> ElementFormatter b)
              -> ElementFormatter b
formatterBind (ElementFormatter s) f = ElementFormatter $ do
    v <- s -- s is a StateT
    getFormatter $ f v

formatterReturn :: a -> ElementFormatter a
formatterReturn a = ElementFormatter $ return a

-- | The 'MonadIO' instance
instance MonadIO ElementFormatter where
    liftIO m = ElementFormatter $! liftIO m

-- | Return the underlying modifiable Element
elementFormatterGet :: ElementFormatter (Ptr ElementImpl)
elementFormatterGet = do
    e <- ElementFormatter $! get
    return $! getElementImpl (_elementWrittable e)

-- | Set the specified value ('v') at the current level in the
-- ElementFormatter. Note that this method will *not* work and will thrown
-- on exception, if the Element at the current level is not a Simple Type.
-- A simple type is one that is not an array or a complex object (sub
-- element)
setValue :: T.Value -> ElementFormatter ()
setValue v = setValueAt v 0

-- | Append the specified value 'v' at the current level of the Element.
-- This method will *not* work unless the current level is an array of
-- simple types
appendValue :: T.Value -> ElementFormatter ()
appendValue v = setValueAt v 0xffffffff

-- | Same as 'appendValue', but the value 'et' can be set at the particular
-- index 'i'. It is undefined to set the value at a index that has not been
-- 'appendValue'-d before.
setValueAt :: T.Value -> CInt -> ElementFormatter ()
setValueAt et i = do
    elemPtr <- elementFormatterGet
    --withCString sName $ \s ->
    ret <- liftIO $
     case et of
        (T.BlpBool v) ->  blpapi_Element_setValueBool elemPtr v i
        (T.BlpChar v) ->  blpapi_Element_setValueChar elemPtr v i
        (T.BlpInt32 v) ->  blpapi_Element_setValueInt32 elemPtr v i
        (T.BlpInt64 v) ->  blpapi_Element_setValueInt64 elemPtr v i
        (T.BlpFloat v) ->  blpapi_Element_setValueFloat32 elemPtr v i
        (T.BlpDouble v) ->  blpapi_Element_setValueFloat64 elemPtr v i
        (T.BlpString v) -> withCString (Text.unpack v) $ \s ->
            blpapi_Element_setValueString elemPtr s i
        --(T.BlpDateTime v) -> alloca $ \dPtr -> do
        --    poke dPtr v
        --    blpapi_Element_setValueHighPrecisionDatetime elemPtr dPtr i
    when (0 /= ret) $ liftIO $! getErrorString ret >>= fail

-- | Return a Write-only element at the specified name 's'.
getElement :: String -> ElementFormatter ElementWritable
getElement s = do
    elemPtr <- elementFormatterGet
    ne <- liftIO $ alloca $ \newElem -> join $
            withCString s $ \cs -> do
                rc <- blpapi_Element_getElement elemPtr newElem cs nullPtr
                if 0 == rc
                then return (peek newElem)
                else getErrorString rc >>= fail
    return $ ElementWritable $! ne

-- | Append an element to the array of element and apply the specified
-- 'ElementFormatter' 'ef'. This method will *not* work unless the current
-- level is an array of Complex Types and an exception will be thrown.
appendElement :: ElementFormatter a -> ElementFormatter ()
appendElement ef = do
    elemPtr <- elementFormatterGet
    ne <- liftIO $ alloca $ \newElem -> do
            rc <- blpapi_Element_appendElement elemPtr newElem
            if 0 == rc
            then peek newElem
            else fail "Failed to append"
    liftBlpapi $! formatElementWritable (ElementWritable ne) ef

-- | Return the writable element from the specified 'Request' 'r'.
getElementWritable :: Request -> Blpapi ElementWritable
getElementWritable r = do
    newElem <- liftIO $! withForeignPtr rFPtr blpapi_Request_elements
    return $! ElementWritable $! newElem
    where rFPtr = getImpl r

-- | Format the specified 'Request' 'r' with the specified
-- 'ElementFormatter' 'f'.
formatRequest :: Request -> ElementFormatter a -> Blpapi ()
formatRequest r f = do
    e <- getElementWritable r
    formatElementWritable e f

-- | Format a 'ElementWritable' with an 'ElementFormatter'
formatElementWritable :: ElementWritable -> ElementFormatter a -> Blpapi ()
formatElementWritable element formatter = do
    evalStateT (getFormatter formatter) $ ElementFormatterState element
    return ()

-- | Format the sub element with the specified name 'n' with the specified
-- 'ElementFormatter' 'ef'. This method will *not* work and an exception
-- will be thrown unless the sub element is of complex type.
formatSubElement :: String -> ElementFormatter a -> ElementFormatter ()
formatSubElement n ef = do
        fields <- getElement n
        liftBlpapi $! formatElementWritable fields ef

