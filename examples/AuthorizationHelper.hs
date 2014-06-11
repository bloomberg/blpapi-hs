{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE OverloadedStrings #-}
module AuthorizationHelper where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.Trans             (liftIO)
import           Data.Maybe
import qualified Data.Text                       as T
import           Finance.Blpapi.ElementFormatter
import           Finance.Blpapi.ElementParser    as P
import           Finance.Blpapi.Event
import           Finance.Blpapi.Session
import           Finance.Blpapi.Types            as BT

import           Options.Applicative

-- | This should go in a different utility, but currently placing it here
throwOnError :: (Either String a) -> Blpapi a
throwOnError (Right a) = return a
throwOnError (Left str) = fail str

data AuthenticationType = User
                          | App !String
                          | UserApp !String
                          | Dir !String
                          | None
                deriving (Show)

authUser :: String
authUser = "AuthenticationType=OS_LOGON"
authAppPrefix :: String
authAppPrefix = "AuthenticationMode=APPLICATION_ONLY;"
    ++ "ApplicationAuthenticationType=APPNAME_AND_KEY;ApplicationName="
authUserAppPrefix :: String
authUserAppPrefix = "AuthenticationMode=USER_AND_APPLICATION;"
            ++ "AuthenticationType=OS_LOGON;"
            ++ "ApplicationAuthenticationType=APPNAME_AND_KEY;ApplicationName="
authDirPrefix :: String
authDirPrefix = "AuthenticationType=DIRECTORY_SERVICE;DirSvcPropertyName="


authParser :: Parser AuthenticationType
authParser = nullOption
  ( long "auth"
   <> value User
   <> metavar "AuthOptions"
   <> eitherReader parseAutParserString
   <> help ("authentication option: "
         ++ "user|none|app=<app>|userapp=<app>|dir=<property> (default: user)")
   )

parseAutParserString :: String -> Either String AuthenticationType
parseAutParserString "user" = Right User
parseAutParserString "none" = Right None
parseAutParserString ('a':'p':'p':'=':xs) = Right (App xs)
parseAutParserString ('u':'s':'e':'r':'a':'p':'p':'=':xs) = Right (App xs)
parseAutParserString ('d':'i':'r':'=':xs) = Right (App xs)
parseAutParserString _ = Left "Bad Auth Argument"

getAuthType :: Maybe String -> AuthenticationType
getAuthType Nothing = User
getAuthType (Just s) = undefined

getAuthString :: AuthenticationType -> String
getAuthString User = authUser
getAuthString (App s) = authAppPrefix ++ s
getAuthString (UserApp s) = authUserAppPrefix ++ s
getAuthString (Dir s) = authDirPrefix ++ s
getAuthString _ = ""

extractToken :: Message -> Either String T.Text
extractToken m =
  P.getElement "TokenGenerationSuccess" (messageData m)
     >>= P.getElement "token"
     >>= P.getValue

populateAuthRequest :: T.Text -> Request -> Blpapi ()
populateAuthRequest token req =
  formatRequest req
    $! formatSubElement "token"
    $! setValue (BT.BlpString token)

noopEventHandler :: Event -> Blpapi ()
noopEventHandler _ = return ()

data AuthHandlers = AuthHandlers {
  authHandlerResult :: Maybe SessionHandler,
  authHandlerUpdate :: Maybe SessionHandler
}

data AuthState = SendinAuth | AuthSuccess | AuthTerminated

isMessageAuthFailure :: Message -> Bool
isMessageAuthFailure m
  = case P.getElement "AuthorizationFailure" (messageData m) of
      Left str -> False
      Right _ -> True
authorizationEventHandler :: MVar AuthState
                          -> Chan (Either String ())
                          -> AuthHandlers
                          -> Event
                          -> Blpapi ()
authorizationEventHandler m ch (AuthHandlers r u) e = do
  authState <- liftIO $ takeMVar m
  case authState of
    SendinAuth -> do
      fromMaybe noopEventHandler r e
      liftIO $ if isMessageAuthFailure (eventContent e)
      then do
        putMVar m AuthTerminated
        writeChan ch $ Left $ "AuthFailed: " ++ show e
      else do
        putMVar m AuthSuccess
        writeChan ch $ Right ()
      return ()
    AuthSuccess -> fromMaybe noopEventHandler u e
    AuthTerminated -> return ()

tokenEventHandler :: Chan (Either String T.Text) -> Event -> Blpapi ()
tokenEventHandler ch e = do
  let tokenResult = extractToken (eventContent e)
  liftIO $ writeChan ch tokenResult

setupAuthorize :: AuthHandlers -> Blpapi (Either String Identity)
setupAuthorize handlers = do
  authService <- openService "//blp/apiauth" >>= throwOnError
  tokenChannel <- liftIO newChan
  generateToken' Nothing $ tokenEventHandler tokenChannel
  tokenMessage <- liftIO $ readChan tokenChannel
  iden <- createIdentity
  case tokenMessage of
    Left str -> return $ Left $ "Failed to get Token: " ++ show tokenMessage
    Right token -> do
      authReq <- createAuthorizationRequest authService >>= throwOnError
      populateAuthRequest token authReq
      authChan <- liftIO newChan
      authState <- liftIO $ newMVar SendinAuth
      sendAuthorizationRequest' authReq iden Nothing
                      (authorizationEventHandler authState authChan handlers)
      authResult <- liftIO $ readChan authChan
      case authResult of
        Left err -> return $ Left err
        Right () -> return $ Right iden

authorize' :: Blpapi (Either String Identity)
authorize' = setupAuthorize (AuthHandlers Nothing Nothing)

