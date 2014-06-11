{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Finance.Blpapi.Session
Description : The Blpapi session
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

There are two types of data that can be consumed through
the session: subscription data and request/response data.

Subscription data is data that comes out-of-band. The data will keep
flowing as long as there is a subscription channel. An important thing
to note about Subscription data is that it can be dropped at any part of
the infrastructure (which will cause a DataLoss to be generated).

Request/response data is data that is received due to a solicited
request to the infrastructure. Every request will have a final response
(there could be partial responses before a final response) which marks
the end of the request. Request/response data is not dropped by the
infrastructure and a final response (or Timeout) is guaranteed for a
request.

Session operate in an Asynchronous mode. In this mode Session gets access
to data through a callback handle ('SessionHandler') passed
during the construction of Session.

Several methods in Session take a CorrelationId parameter. The
application may choose to supply its own CorrelationId values
or allow the Session to create values. If the application
supplies its own CorrelationId values it must manage their
lifetime such that the same value is not reused for more than
one operation at a time. The lifetime of a CorrelationId begins
when it is supplied in a method invoked on a Session and ends
either when it is explicitly canceled using 'cancel'() or 'unsubscribe'(),
when a 'EventTypeResponse' 'Event' (not a 'EventTypePartialResponse')
containing it is received or when a 'EventTypeSubscriptionStatus' 'Event'
which indicates that the subscription it refers to has been terminated (or
failed) is received.

Many methods in Session operate in two modes:

  1. Synchronous Methods (Blocking methods): These methods guarantee that
    the request has been processed by the library and the infrastructure
    has responded. When the method returns, the return code will tell the
    status of the operation (Success or Failure).

  2. Asynchronous Methods (Non-blocking methods): These methods guarantee
    that the library has accepted or rejected the request for the
    specified operation. If the request for the operation has been
    accepted, the result would be delivered in the form of 'Event'(s).
    When using a Session in push mode the application must be aware that
    because the callbacks are invoked from another thread they may be
    processed before the call which generates them has returned.
-}

module Finance.Blpapi.Session (
  MonadBlpapi(..),
  Blpapi,
  SessionHandler,
  MessageHandler,

  runBlpapi,
  stopBlpapi,
  createSession,
  openService,
  createRequest,

  createAuthorizationRequest,
  createIdentity,

  generateToken,
  generateToken',
  sendAuthorizationRequest,
  sendAuthorizationRequest',

  sendRequest,
  sendRequest',
  sendRequestWithIdentity,
  sendRequestWithIdentity',

  subscribe,
  subscribeWithIdentity,

  cancel,
  unsubscribe,

  getElementFromRequest,
  isEventSessionTerminated,

  Identity,

-- From Finance.Blpapi.CorrelationId
  CorrelationId,
  createCorrelationId,
  createCorrelationIdWithClassId,

-- From Finance.Blpapi.Request
  Request

  ) where

import           Foreign
import           Foreign.C.String

import           Finance.Blpapi.CorrelationId
import           Finance.Blpapi.ElementParser
import           Finance.Blpapi.Event
import           Finance.Blpapi.Identity
import           Finance.Blpapi.Impl.ElementImpl
import           Finance.Blpapi.Impl.ErrorImpl
import           Finance.Blpapi.Impl.IdentityImpl
import           Finance.Blpapi.Impl.RequestImpl
import           Finance.Blpapi.Impl.ServiceImpl
import           Finance.Blpapi.Impl.SessionImpl
import           Finance.Blpapi.Impl.SessionOptionsImpl
import           Finance.Blpapi.Request
import           Finance.Blpapi.Service
import           Finance.Blpapi.SessionOptions
import           Finance.Blpapi.Subscription

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar            ()
import qualified Data.Map                               as Map

import           Data.IORef

-- | 'MonadBlpapi' is a type class, analogous to 'MonadIO' for 'IO', that makes
-- it easy to wrap 'Blpapi' inside monad transformers.
class (Monad m, MonadIO m) => MonadBlpapi m where
    liftBlpapi :: Blpapi a -> m a

instance MonadBlpapi m => MonadBlpapi (StateT s m) where
    liftBlpapi m = lift $! liftBlpapi m

data BlpapiResult a = BlpapiValue a |
                      NoMoreProcessing String

type StateRef = IORef BlpapiState

newtype BlpapiSession = BlpapiSession {
    unBlpapiSession :: Maybe SessionImplHandle
}

-- | 'Blpapi' is the 'Monad' that users communicate with the infrastructure
-- to get data.
--
-- 'Blpapi' is a monad transformer over 'IO', so all the 'IO' operations
-- can be performed using 'liftIO'
-- @
--     test :: Blpapi ()
--     test = liftIO $ putStrLn "Hello World!"
-- @
newtype Blpapi a = Blpapi {
      unBlpapi :: RWST (IORef BlpapiState) () ()  IO (BlpapiResult a)
}

type ServiceMap = Map.Map Service (ForeignPtr ServiceImpl)

data CidHandlerMap = CidHandlerMap{
  _lastCid :: !Int,
  _cidMap  :: Map.Map CorrelationId MessageHandler
}

data BlpapiState = BlpapiState {
      --_blpapiLogger :: String -> IO (),
      _blpapiSesssionImpl          :: BlpapiSession,
      _blpapiCorrelationHandlerMap :: TVar CidHandlerMap,
      _blpapiServiceMap            :: ServiceMap
}

instance Monad Blpapi where
    (>>=) = blpapiBind
    return = blpapiReturn
    fail = blpapiFail

blpapiBind :: Blpapi a -> (a -> Blpapi b) -> Blpapi b
blpapiBind (Blpapi s) f =
  Blpapi $ do
      res <- s -- s is a readerT
      case res of
        BlpapiValue a -> unBlpapi $ f a
        NoMoreProcessing m -> return $! NoMoreProcessing m

blpapiReturn :: a -> Blpapi a
blpapiReturn = Blpapi . return . BlpapiValue

blpapiFail :: String -> Blpapi a
blpapiFail m = Blpapi $! return $! NoMoreProcessing m


instance MonadIO Blpapi where
    liftIO m = Blpapi $! liftM BlpapiValue $! liftIO m

instance MonadBlpapi Blpapi where
    liftBlpapi = id

-- | Handler for 'Event's that the SDK will generate for session specific
-- events
type SessionHandler =  Event -> Blpapi ()

-- | Handler for 'Event's that the SDK will generate for data specific
-- events
type MessageHandler =  Event -> Blpapi ()

type ServiceName = String


bmodify :: (BlpapiState -> BlpapiState) -> Blpapi ()
bmodify f = do
  stateVar <- Blpapi $ liftM BlpapiValue ask
  Blpapi $ lift $ atomicModifyIORef stateVar (\x -> (f x , BlpapiValue ()))

bget :: Blpapi BlpapiState
bget = do
  stateVar <- Blpapi $ liftM BlpapiValue ask
  Blpapi $ liftM BlpapiValue $ lift $ readIORef stateVar

bgetStateRef :: Blpapi StateRef
bgetStateRef = Blpapi $ liftM BlpapiValue ask

-- | Attempt to start and blocks until the session has started or failed.
--
-- Once session is started or the session failed to start the
-- 'SessionHandler' is invoked with 'EventTypeSessionStatus' 'Event'. If the
-- session failed to start return 'Left' with the error string
--
-- A session may by started only once.
createSession :: SessionOptions
              -> SessionHandler
              -> Blpapi (Either String BlpapiSession)
createSession o h = do
  oImpl <- liftIO $ convertSessionOptionsImpl o
  sessionHandle <- liftIO $ withForeignPtr oImpl newSessionImpl
  bmodify (\s -> s {_blpapiSesssionImpl = BlpapiSession $ Just sessionHandle})
  st <- bgetStateRef
  liftIO $ forkIO $ getNextEventIO (Just sessionHandle) h st

  rc <- liftIO $ withForeignPtr sessionHandle blpapi_Session_start
  if rc == 0
  then return $ Right $ BlpapiSession $ Just sessionHandle
  else liftIO $ liftM Left $ getErrorString rc

correlationIdClassForRequest :: Int
correlationIdClassForRequest = 1

createNewCorrelationIfNeeded :: Maybe CorrelationId -> Blpapi CorrelationId
createNewCorrelationIfNeeded (Just cid) = return cid
createNewCorrelationIfNeeded Nothing = do
  st <- bget
  let tvar = _blpapiCorrelationHandlerMap st
  liftIO $ atomically $ do
    cidHandler <- readTVar tvar
    let lastCid = _lastCid cidHandler
    writeTVar tvar (cidHandler {_lastCid = lastCid + 1})
    return
      $ createCorrelationIdWithClassId correlationIdClassForRequest lastCid

getHandlerForCid :: CorrelationId -> Blpapi (Maybe MessageHandler)
getHandlerForCid cid = do
  st <- bget
  hm <- liftIO $ readTVarIO (_blpapiCorrelationHandlerMap st)
  return $ Map.lookup cid (_cidMap hm)

addCidAndHandlerToMap' :: CorrelationId
                       -> MessageHandler
                       -> TVar CidHandlerMap
                       -> STM()
addCidAndHandlerToMap' cid handler tvar =
  modifyTVar' tvar $ \cm -> cm {_cidMap = Map.insert cid handler (_cidMap cm)}

addCidAndHandlerToMap :: CorrelationId -> MessageHandler -> Blpapi()
addCidAndHandlerToMap cid handler = do
  st <- bget
  liftIO $ atomically $
      addCidAndHandlerToMap' cid handler (_blpapiCorrelationHandlerMap st)
  return ()

emptyCidHandler :: CidHandlerMap
emptyCidHandler = CidHandlerMap 1 Map.empty

-- | Runs a 'Blpapi' monad action in the 'IO' monad
runBlpapi :: Blpapi () -> IO ()
runBlpapi m = do
  handlerState <- newTVarIO emptyCidHandler
  ref <- newIORef (BlpapiState (BlpapiSession Nothing) handlerState Map.empty)
  evalRWST (unBlpapi (runBlpapi' m)) ref ()
  return ()

runBlpapi' :: Blpapi () -> Blpapi ()
runBlpapi' m = do
  m
  stopBlpapi

-- | Stop the session that was established to the infrastructure backend.
-- This will also stop delivery of events (once the current event being
-- handled is done)
stopBlpapi :: Blpapi ()
stopBlpapi = do
  s <- getSessionHandle
  liftIO $ stopImpl s
  bmodify (\ses -> ses {_blpapiSesssionImpl = BlpapiSession Nothing})

getSessionHandle :: Blpapi SessionImplHandle
getSessionHandle = do
  st <- bget
  getSessionHandle' $ unBlpapiSession $ _blpapiSesssionImpl st

getServiceMap :: Blpapi ServiceMap
getServiceMap = do
  st <- bget
  return $ _blpapiServiceMap st

putServiceMap :: ServiceMap -> Blpapi ()
putServiceMap m = bmodify (\st -> st { _blpapiServiceMap = m })

getSessionHandle' :: Maybe SessionImplHandle -> Blpapi SessionImplHandle
getSessionHandle' (Just s) = return s
getSessionHandle' Nothing = blpapiFail "Session Not Active"

-- | Attempt to open the service identified by 'ServiceName' and block
-- until the service is either opened successfully or has failed. On
-- failure return 'Left' with error string otherwise return the 'Service'
-- that was opened as 'Right'. This method may generate an 'Event' in the
-- 'SessionHandler' which was registered during 'Session' creation.
openService :: ServiceName -> Blpapi (Either String Service)
openService s = do
  st <- getSessionHandle
  ans <- liftIO $ openServiceImpl st s
  case ans of
    Right (ser, ptrS) -> do
      m <- getServiceMap
      putServiceMap (Map.insert ser ptrS m)
      return $ Right ser
    Left str -> return $ Left str

-- | Begin subscriptions for each entry in the specified
-- 'subscription' list.
--
-- A 'EventTypeSubscriptionStatus' Event will be generated for each entry
-- in the 'subscriptionList'.
subscribe :: [Subscription] -> Blpapi ()
subscribe s = do
   st <- getSessionHandle
   liftIO $ subscribeImpl st s Nothing
   return ()

-- | Begin subscriptions for each entry in the specified
-- 'subscription' list using the specified 'Identity' for
-- authorization.
--
-- A 'EventTypeSubscriptionStatus' Event will be generated for each entry
-- in the 'subscriptionList'.
subscribeWithIdentity :: [Subscription] -> Identity -> Blpapi ()
subscribeWithIdentity s iden = do
   st <- getSessionHandle
   liftIO $ subscribeImpl st s $ Just (identityImpl iden)
   return ()

-- | Cancel each of the current subscriptions identified by the
-- specified 'Subscription' list. If the correlation ID of any
-- entry in the 'Subscription' list does not identify a current
-- subscription then that entry is ignored. All entries which
-- have valid correlation IDs will be canceled.
--
-- Once this call returns the correlation ids in the
-- 'subscription' list will not be seen in any subsequent
unsubscribe :: [Subscription] -> Blpapi ()
unsubscribe s = do
   st <- getSessionHandle
   liftIO $ unsubscribeImpl st s
   return ()

-- | If the specified 'CorrelationId' identifies a current operation then
-- cancel the operation. The 'CorrelationId' can be reused, when the
-- operation identified by 'CorrelationId' has concluded. The operation
-- can conclude with success (if the cancel was too late) or with
-- failure (in which case the operation will be interrupted).
--
-- Eg. If the 'CorrelationId' identifies a subscription, the
-- 'CorrelationId' can be reused after a 'SubscriptionTerminated' or a
-- 'SubscriptionFailure' has been received with specified
-- 'CorrelationId'
cancel :: [CorrelationId] -> Blpapi ()
cancel cids = do
   st <- getSessionHandle
   liftIO $ forM_ cids (cancelSingleRequest st)
   return ()

getNextEventIO :: Maybe SessionImplHandle
               -> SessionHandler
               -> IORef BlpapiState
               -> IO ()
getNextEventIO Nothing _ _ = return ()
getNextEventIO (Just sessionHandle) h st = do
  (newHandle, _) <- evalRWST (unBlpapi $ getNextEvent sessionHandle h) st ()
  getNextEventIO (temp newHandle) h st
    where
      temp (BlpapiValue a) = Just a
      temp (NoMoreProcessing _) = Nothing

getCidsFromEvent :: Event -> [CorrelationId]
getCidsFromEvent e = messageCorrelationIds $ eventContent e

getNextEvent :: SessionImplHandle -> SessionHandler -> Blpapi SessionImplHandle
getNextEvent h fn = do
  eList <- liftIO $ getNextEventImpl h
  terminated <- foldM (\ acc e -> do
    --needToSend <- forM (getCidsFromEvent e) $ \cid -> do
    _ <- forM (getCidsFromEvent e) $ \cid -> do
      handler <- getHandlerForCid cid
      case handler of
        Just cidH -> cidH e >> return True
        Nothing -> return False
    --unless (and needToSend) $ fn e
    fn e
    return (if isEventSessionTerminated e then acc || True else acc)
    ) False eList
  if terminated
    then getSessionHandle' Nothing --TODO
    else getSessionHandle

-- | Returns a empty 'Request' for the specified operation 'String' on the
-- specified 'Service'. If 'operation' does not identify a valid operation
-- in the 'Service' or if some other error occurs, return 'Left' with the
-- appropriate error string.
--
-- An application must populate the 'Request' (using 'ElementFormatter')
-- before 'sendRequest'.
createRequest :: Service -> String -> Blpapi (Either String Request)
createRequest s oName =
  createRequestImpl s
      (\sPtr reqPtr ->
            withCString oName $ \ocName ->
                blpapi_Service_createRequest sPtr reqPtr ocName)

-- | Returns an empty 'Request' object for operation
-- 'AuthorizationRequest'. If the 'authorizationOperation' does not
-- identify a valid operation for this 'Service' then return 'Left' with
-- appropriate error string.
--
-- An application must populate the 'Request' (using 'ElementFormatter')
-- before 'sendRequest'.
createAuthorizationRequest :: Service -> Blpapi (Either String Request)
createAuthorizationRequest s =
  createRequestImpl s (\sPtr reqPtr ->
                blpapi_Service_createAuthorizationRequest sPtr reqPtr nullPtr)

createRequestImpl :: forall a. Service
                  -> (Ptr ServiceImpl -> Ptr (Ptr RequestImpl) -> IO Int)
                  -> Blpapi (Either String Request)
createRequestImpl s f = do
    m <- getServiceMap
    liftIO $ withForeignPtr (getServiceImpl m s) $ \sPtr ->
        alloca $ \reqPtr -> do
                rc <- f sPtr reqPtr
                if rc /= 0
                then liftM Left $ getErrorString rc
                else do
                  rPtr <- peek reqPtr
                  reqFPtr <- newForeignPtr blpapi_Request_destroy rPtr
                  return $ Right $ Request reqFPtr
    where
        getServiceImpl m ser = getServiceImpl' $ Map.lookup ser m
        getServiceImpl' (Just sImpl) = sImpl


sendRequestWithIdentityImpl' :: Request
                             -> Maybe CorrelationId
                             -> MessageHandler
                             -> Maybe (ForeignPtr IdentityImpl)
                             -> Blpapi ()
sendRequestWithIdentityImpl' r cid h idenM = do
  newCid <- createNewCorrelationIfNeeded cid
  addCidAndHandlerToMap newCid h
  sendRequestWithIdentityImpl r (Just newCid) idenM

sendRequestWithIdentityImpl :: Request
                            -> Maybe CorrelationId
                            -> Maybe (ForeignPtr IdentityImpl)
                            -> Blpapi ()
sendRequestWithIdentityImpl r cidM idenM = do
    st <- getSessionHandle
    liftIO $ sendRequestImpl st r cidM idenM
    return ()

-- | Similar to 'sendRequest\'' but an 'Identity' can be provided on
-- the 'Request'.
sendRequestWithIdentity' :: Request
                         -> Maybe CorrelationId
                         -> Identity
                         -> SessionHandler
                         -> Blpapi ()
sendRequestWithIdentity' r cid iden h
  = sendRequestWithIdentityImpl' r cid h $ Just (identityImpl iden)

-- | Similar to 'sendRequest' but an 'Identity' can be provided to send to
-- the 'Request'.
sendRequestWithIdentity :: Request
                        -> Maybe CorrelationId
                        -> Identity
                        -> Blpapi ()
sendRequestWithIdentity r cidM iden
  = sendRequestWithIdentityImpl r cidM $ Just (identityImpl iden)

-- | Similar to 'sendRequest' except that the 'Event's are delivered using
-- the specified 'MessageHandler'.
sendRequest' :: Request -> Maybe CorrelationId -> MessageHandler -> Blpapi ()
sendRequest' r cid h
  = sendRequestWithIdentityImpl' r cid h Nothing

-- | Send the specified 'Request'. If the optionally specified
-- 'CorrelationId' is supplied use it.
--
-- A successful request will generate zero or more
-- 'EventTypePartialResponse' 'Message' followed by exactly one
-- 'EventTypeResponse' 'Message'. Once the final 'EventTypeResponse' 'Message'
-- has been received the 'CorrelationId' associated with this 'Request' may
-- be re-used. If the request fails at any stage a 'EventTypeRequestStatus'
-- will be generated after which the 'CorrelationId' associated with the
-- 'Request' may be re-used. All the 'Event's are delivered using the
-- 'SessionHandler' that was registered during 'Session' creation.
sendRequest :: Request -> Maybe CorrelationId -> Blpapi ()
sendRequest r cid
  = sendRequestWithIdentityImpl r cid Nothing

-- | Similar to 'sendAuthorizationRequest' except that the messages are
-- returned in specified 'MessageHandler''
sendAuthorizationRequest' :: Request
                          -> Identity
                          -> Maybe CorrelationId
                          -> MessageHandler
                          -> Blpapi ()
sendAuthorizationRequest' r i cid h = do
  newCid <- createNewCorrelationIfNeeded cid
  addCidAndHandlerToMap newCid h
  sendAuthorizationRequest r i (Just newCid)

-- | Send the specified 'Request' and update the specified
-- 'Identity' with the results asynchronously. If the optionally specified
-- 'CorrelationId' is supplied, it is used.
--
-- Authorization can be done in one of the two modes:
--
--     1. VC Mode: This kind of authorization establishes a Virtual
--        Circuit if the authorization succeeds, in which case an event
--        of type 'EventTypePartialResponse' (with message
--        'AuthorizationSuccess' for '//blp/apiauth') is generated. All
--        subsequent updates to authorization like entitlement changes
--        are delivered as 'EventTypePartialResponse'. Once the
--        authorization is terminated (revoked) an event of
--        'EventTypeResponse' (with messageType
--        'AuthorizationRevoked' for '//blp/apiauth') is generated . In
--        case of authorization failure an event type
--        'EventTypeResponse' (with messageType
--        'AuthorizationFailure' for '//blp/apiauth') is generated. The
--        'CorrelationId' provided can not be reused until an
--        event of 'EventTypeResponse' is received, which marks the
--        end of the operation.
--
--     2. Non VC Mode: This kind of authorization does not form a
--        virtual circuit and receipt of an event with event type
--        'EventTypeResponse' determines a failure (messageType
--        'AuthorizationFailure' for '//blp/apiauth') or success
--        (messageType 'AuthorizationSuccess'). If the authorization
--        succeeds all future updates to authorization are delivered as
--        event type 'EventTypeAuthorizationStatus'. The
--        correlationId that is returned can not be reused until an
--        event of 'EventTypeResponse' is received with failure or an
--        event of 'EventTypeAuthorizationStatus' with messageType
--        'AuthorizationRevoked' is received, which marks the end of
--        the operation.
--
-- The 'Identity' supplied must have been returned from this Session's
-- 'createIdentity'() method. Note that all the 'Event's are returned in the
-- 'SessionHandler' that was registered during 'Session' creation.
sendAuthorizationRequest :: Request
                         -> Identity
                         -> Maybe CorrelationId
                         -> Blpapi ()
sendAuthorizationRequest r (Identity idPtr) cid = do
    st <- getSessionHandle
    liftIO $ sendAuthRequestImpl st idPtr r cid
    return ()

-- | Generate a token asynchronously to be used for authorization using the
-- optionally specified 'CorrelationId'. The 'CorrelationId' can not be
-- reused (and can be canceled) until a 'EventTypeTokenStatus' with
-- message 'TokenGenerationSuccess' or 'TokenGenerationFailure' has been
-- received in the specified 'MessageHandler', which marks the end of this
-- operation.
--
-- If invalid authentication option is specified in session option or there
-- is failure to get authentication information based on authentication
-- option, then an 'Exception' is thrown.
generateToken' :: Maybe CorrelationId -> MessageHandler -> Blpapi ()
generateToken' cid h = do
  newCid <- createNewCorrelationIfNeeded cid
  addCidAndHandlerToMap newCid h
  generateToken (Just newCid)

-- | Generate a token asynchronously to be used for authorization using the
-- optionally specified 'CorrelationId'. The 'CorrelationId' can not be
-- reused (and can be canceled) until a 'EventTypeTokenStatus'' with
-- message 'TokenGenerationSuccess' or 'TokenGenerationFailure' has been
-- received in the 'SessionHandler' provided during 'Session' creation,
-- which marks the end of this operation.
--
-- If invalid authentication option is specified in session option or there
-- is failure to get authentication information based on authentication
-- option, then an 'Exception' is thrown.
generateToken :: Maybe CorrelationId -> Blpapi ()
generateToken cid = do
    st <- getSessionHandle
    liftIO $ generateTokenImpl st cid
    return ()

isIdentyAuthorized :: Identity -> Service -> Blpapi Bool
isIdentyAuthorized = undefined

-- | Return an 'Identity' that has not been authorized.
createIdentity :: Blpapi Identity
createIdentity = do
   st <- getSessionHandle
   liftIO $ liftM Identity $ createIdentityImpl st

-- | Return the underlying 'Element' in the specified 'Request'.
getElementFromRequest :: Request -> Blpapi Element
getElementFromRequest (Request reqFPtr) =
    liftIO $ withForeignPtr reqFPtr $ \reqPtr -> do
        blpElem <- blpapi_Request_elements reqPtr
        convertElementImpl blpElem

isEventSessionTerminated :: Event -> Bool
isEventSessionTerminated (Event EventTypeSessionStatus msgList)
  = isMessageSessionTerminatedOrFailed msgList
isEventSessionTerminated _ = False

isMessageSessionTerminated :: Message -> Bool
isMessageSessionTerminated (Message _ _ e _ _)
  = case getElement "SessionTerminated" e of
      Right _ -> True
      _ -> False

isMessageSessionFailed :: Message -> Bool
isMessageSessionFailed (Message _ _ e _ _)
  = case getElement "SessionStartupFailure" e of
      Right _ -> True
      _ -> False

isMessageSessionTerminatedOrFailed :: Message -> Bool
isMessageSessionTerminatedOrFailed m
  = isMessageSessionFailed m || isMessageSessionTerminated m

