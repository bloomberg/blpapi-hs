{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AuthorizationHelper
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans             (liftIO)
import           Data.Monoid
import qualified Data.Text                       as T
import           Finance.Blpapi.ElementFormatter
import qualified Finance.Blpapi.ElementParser    as P
import           Finance.Blpapi.Event
import           Finance.Blpapi.PrettyPrint
import           Finance.Blpapi.Session
import           Finance.Blpapi.SessionOptions
import           Finance.Blpapi.Subscription
import           Finance.Blpapi.Types            as BT
import           Options.Applicative

notifyDone :: MVar () -> Event -> Blpapi ()
notifyDone m e
    | isEventSessionTerminated e = liftIO $! putMVar m ()
    | otherwise = return ()

defaultHandler :: MVar () -> Event -> Blpapi ()
defaultHandler m e = do
  liftIO $! prettyPrint (eventContent e)
  notifyDone m e

getSubscriptionList :: [Subscription]
getSubscriptionList =
  [Subscription (TopicString "IBM US Equity" ["BID", "ASK"]) Nothing Nothing]

setupSubscription :: Identity -> Blpapi ()
setupSubscription iden = do
  openService "//blp/mktdata"
  subscribeWithIdentity getSubscriptionList iden

data CmdOptions = CmdOptions {
  cmdIp   :: String,
  cmdPort :: Int,
  cmdAuth :: AuthenticationType
} deriving (Show)

setupBlpapi :: CmdOptions -> MVar () -> Blpapi ()
setupBlpapi c m = do
  createSession
     (defaultSessionOptions {
         serverAddresses = [ServerAddress (cmdIp c) (cmdPort c)],
         authenticationOptions = getAuthString (cmdAuth c)})
     (defaultHandler m) >>= throwOnError
  authResult <- authorize'
  case authResult of
    Left err -> liftIO $ putStrLn err
    Right iden -> do
      setupSubscription iden
      liftIO $ threadDelay (60*1000*1000)

cmdLineParser :: Parser CmdOptions
cmdLineParser = CmdOptions <$> ipParser <*> portParser <*> authParser
  where
    portParser = option $
        value 8194
        <> long "port"
        <> metavar "Port"
        <> help "server port"
    ipParser = strOption $
        value "localhost"
        <> long "ip"
        <> metavar "IP"
        <> help "server name or IP"

main :: IO ()
main = do
  cmd <- execParser (info (helper <*> cmdLineParser) mempty)
  m <- newEmptyMVar
  runBlpapi $ setupBlpapi cmd m
  liftIO $! takeMVar m
  putStrLn "About to Quit !!"
  return ()
