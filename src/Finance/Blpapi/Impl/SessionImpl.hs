{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Finance.Blpapi.Impl.SessionImpl
Description : FFI for Session
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows
-}
module Finance.Blpapi.Impl.SessionImpl where

import           Control.Monad
import           Finance.Blpapi.Impl.RequestImpl
import           Finance.Blpapi.Impl.SessionOptionsImpl
import           Finance.Blpapi.Request
import           Foreign
import           Foreign.C.String

import           Finance.Blpapi.CorrelationId
import           Finance.Blpapi.Event
import           Finance.Blpapi.Impl.CorrelationIdImpl
import           Finance.Blpapi.Impl.EventImpl
import           Finance.Blpapi.Impl.IdentityImpl
import           Finance.Blpapi.Impl.ServiceImpl
import           Finance.Blpapi.Impl.SubscriptionListImpl
import           Finance.Blpapi.Service
import           Finance.Blpapi.Subscription

import           Finance.Blpapi.Impl.ErrorImpl

newtype SessionImpl = SessionImpl (Ptr SessionImpl)
type SessionImplHandle = ForeignPtr SessionImpl

newtype EventQueueImpl = EventQueueImpl (Ptr EventQueueImpl)

foreign import ccall safe "blpapi_session.h blpapi_Session_create"
        blpapi_Session_create :: Ptr SessionOptionsImpl
                              -> Ptr ()
                              -> Ptr ()
                              -> Ptr ()
                              -> IO (Ptr SessionImpl)

foreign import ccall safe "blpapi_session.h &blpapi_Session_destroy"
        blpapi_Session_destroy :: FunPtr (Ptr SessionImpl -> IO ())

foreign import ccall safe "blpapi_session.h blpapi_Session_start"
        blpapi_Session_start :: Ptr SessionImpl -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_stop"
        blpapi_Session_stop :: Ptr SessionImpl -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_getService"
        blpapi_Session_getService :: Ptr SessionImpl
                                  -> Ptr (Ptr ServiceImpl)
                                  -> CString
                                  -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_openService"
        blpapi_Session_openService :: Ptr SessionImpl -> CString -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_subscribe"
        blpapi_Session_subscribe :: Ptr SessionImpl
                                    -> Ptr SubscriptionListImpl
                                    -> Ptr IdentityImpl
                                    -> CString
                                    -> Int
                                    -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_sendRequest"
        blpapi_Session_sendRequest :: Ptr SessionImpl
                                    -> Ptr RequestImpl
                                    -> Ptr CorrelationIdImpl
                                    -> Ptr IdentityImpl
                                    -> Ptr EventQueueImpl
                                    -> CString
                                    -> Int
                                    -> IO Int

foreign import ccall safe
  "blpapi_session.h blpapi_Session_sendAuthorizationRequest"
        blpapi_Session_sendAuthorizationRequest :: Ptr SessionImpl
                                                -> Ptr RequestImpl
                                                -> Ptr IdentityImpl
                                                -> Ptr CorrelationIdImpl
                                                -> Ptr EventQueueImpl
                                                -> CString
                                                -> Int
                                                -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_nextEvent"
        blpapi_Session_nextEvent :: Ptr SessionImpl
                                 -> Ptr (Ptr EventImpl)
                                 -> Int
                                 -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_createIdentity"
        blpapi_Session_createIdentity :: Ptr SessionImpl
                                      -> IO (Ptr IdentityImpl)

foreign import ccall safe "blpapi_session.h blpapi_Session_generateToken"
        blpapi_Session_generateToken :: Ptr SessionImpl
                                     -> Ptr CorrelationIdImpl
                                     -> Ptr EventQueueImpl
                                     -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_cancel"
        blpapi_Session_cancel :: Ptr SessionImpl
                              -> Ptr CorrelationIdImpl -- Cid array
                              -> Int -- num cids
                              -> CString -- RequestLabel
                              -> Int -- RequestLabel size
                              -> IO Int

foreign import ccall safe "blpapi_session.h blpapi_Session_unsubscribe"
        blpapi_Session_unsubscribe :: Ptr SessionImpl
                                    -> Ptr SubscriptionListImpl
                                    -> CString
                                    -> Int
                                    -> IO Int


createIdentityImpl :: ForeignPtr SessionImpl -> IO (ForeignPtr IdentityImpl)
createIdentityImpl session =
  withForeignPtr session $ \s -> do
    p <- blpapi_Session_createIdentity s
    newForeignPtr blpapi_Identity_release p

subscribeImpl :: ForeignPtr SessionImpl
              -> [Subscription]
              -> Maybe (ForeignPtr IdentityImpl)
              -> IO ()
subscribeImpl session subList idPtr =
  withForeignPtr session $ \s -> do
    subListImplF <- populateSubscriptionListImpl subList
    withForeignPtr subListImplF $ \sl -> do
      rc <- case idPtr of
        Just idenPtr ->
          withForeignPtr idenPtr $ \iden ->
           blpapi_Session_subscribe s sl iden nullPtr 0
        Nothing ->
           blpapi_Session_subscribe s sl nullPtr nullPtr 0
      failIfBadErrorCode rc

unsubscribeImpl :: ForeignPtr SessionImpl
              -> [Subscription]
              -> IO ()
unsubscribeImpl session subList =
  withForeignPtr session $ \s -> do
    subListImplF <- populateSubscriptionListImpl subList
    withForeignPtr subListImplF $ \sl -> do
       rc <- blpapi_Session_unsubscribe s sl nullPtr 0
       failIfBadErrorCode rc

populateSubscriptionListImpl :: [Subscription]
                                -> IO (ForeignPtr SubscriptionListImpl)
populateSubscriptionListImpl xs = do
    fsubsListImpl <- newSubscriptionListImpl
    withForeignPtr fsubsListImpl (addSubscriptionToSubscriptionHandle xs)
    return fsubsListImpl

getCidImpl :: Maybe CorrelationId -> CorrelationIdImplWrapped
getCidImpl (Just cid) = convertToCorrelationIdImpl cid
getCidImpl Nothing = createUnsetCorrelationIdImpl


withMaybeCid :: Maybe CorrelationId -> (Ptr CorrelationIdImpl -> IO a) -> IO a
withMaybeCid cid f =
    alloca $ \cidPtr -> do
      poke cidPtr (getCidImpl cid)
      cidImpl <- newCorrelationIdImpl
      withForeignPtr cidImpl $ \cidImplP -> do
        blpapi_CorrelationId_convertFromWrapped cidPtr cidImplP
        f cidImplP

addToSubscriptionList :: Ptr SubscriptionListImpl
                         -> TopicString
                         -> Maybe CorrelationId
                         -> IO Int
addToSubscriptionList lst (QualifiedTopicString s) cid =
    alloca $ \cidPtr -> do
      poke cidPtr (getCidImpl cid)
      cidImpl <- newCorrelationIdImpl
      withForeignPtr cidImpl $ \cidImplP -> do
        blpapi_CorrelationId_convertFromWrapped cidPtr cidImplP
        withCString s $ \cs ->
             blpapi_SubscriptionList_add lst cs cidImplP nullPtr nullPtr 0 0

addToSubscriptionList lst ts cid =
  addToSubscriptionList lst (convertToQualifiedString ts) cid

addSubscriptionToSubscriptionHandle :: [Subscription]
                                        -> Ptr SubscriptionListImpl
                                        -> IO Int
addSubscriptionToSubscriptionHandle  xs p =
      foldM (\_ s ->
                addToSubscriptionList p
                                      (subscriptonString s)
                                      (subscriptionCorrelation s)) 0 xs

getNextEventImpl :: SessionImplHandle -> IO [Event]
getNextEventImpl h = withForeignPtr h getNextEventImpl'

getNextEventImpl' :: Ptr SessionImpl -> IO [Event]
getNextEventImpl' ptr =
  alloca $
    \p -> blpapi_Session_nextEvent ptr p 0 >> peek p >>= convertEventImpl

newSessionImpl :: Ptr SessionOptionsImpl -> IO (ForeignPtr SessionImpl)
newSessionImpl so = do
  rawPtr <- blpapi_Session_create so nullPtr nullPtr nullPtr
  newForeignPtr blpapi_Session_destroy rawPtr


stopImpl :: ForeignPtr SessionImpl -> IO Int
stopImpl session = withForeignPtr session $ \p -> blpapi_Session_stop p

openServiceImpl :: ForeignPtr SessionImpl
                -> String
                -> IO (Either String (Service, ForeignPtr ServiceImpl))
openServiceImpl session serName =
    withCString serName $ \cs -> do
      withForeignPtr session (`blpapi_Session_openService` cs)
      getService session cs


getService :: ForeignPtr SessionImpl
           -> CString
           -> IO (Either String (Service, ForeignPtr ServiceImpl))
getService s serName = withForeignPtr s $ \p ->
     alloca $ \rHandle -> do
       rc <- blpapi_Session_getService p rHandle serName
       if (rc /= 0)
       then getErrorString rc >>= (return . Left)
       else do
         sImpl <- peek rHandle
         blpapi_Service_addRef sImpl
         ser <- convertServiceImpl sImpl
         fsImpl <- newForeignPtr blpapi_Service_release sImpl
         return $ Right (ser, fsImpl)

sendRequestImpl :: ForeignPtr SessionImpl
                -> Request
                -> Maybe CorrelationId
                -> Maybe (ForeignPtr IdentityImpl)
                -> IO ()
sendRequestImpl s req cid idPtr = do
    rc <- withForeignPtr s $ \p ->
      withForeignPtr (getImpl req) $ \r ->
        withMaybeCid cid $ \cidPtr ->
          case idPtr of
            Just idenPtr ->
              withForeignPtr idenPtr $ \iden ->
                blpapi_Session_sendRequest p r cidPtr iden nullPtr nullPtr 0
            Nothing ->
                blpapi_Session_sendRequest p r cidPtr nullPtr nullPtr nullPtr 0
    failIfBadErrorCode rc

sendAuthRequestImpl :: ForeignPtr SessionImpl
                    -> ForeignPtr IdentityImpl
                    -> Request
                    -> Maybe CorrelationId
                    -> IO ()
sendAuthRequestImpl s i req cid =
    withForeignPtr s $ \p ->
      withForeignPtr i $ \iden ->
        withForeignPtr (getImpl req) $ \r ->
          withMaybeCid cid $ \cidPtr -> do
              rc <- blpapi_Session_sendAuthorizationRequest
                         p r iden cidPtr nullPtr nullPtr 0
              failIfBadErrorCode rc

generateTokenImpl :: ForeignPtr SessionImpl -> Maybe CorrelationId -> IO ()
generateTokenImpl s cid =
    withForeignPtr s $ \p ->
          withMaybeCid cid $ \cidPtr -> do
            rc <- blpapi_Session_generateToken p cidPtr nullPtr
            failIfBadErrorCode rc

cancelSingleRequest :: ForeignPtr SessionImpl -> CorrelationId -> IO ()
cancelSingleRequest s cid =
    withForeignPtr s $ \p ->
          withMaybeCid (Just cid) $ \cidPtr -> do
            rc <- blpapi_Session_cancel p cidPtr 1 nullPtr 0
            failIfBadErrorCode rc

