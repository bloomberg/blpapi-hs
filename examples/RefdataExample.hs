{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AuthorizationHelper
import           Finance.Blpapi.ElementFormatter
import qualified Finance.Blpapi.ElementParser    as P
import           Finance.Blpapi.Event
import           Finance.Blpapi.PrettyPrint
import           Finance.Blpapi.Session
import           Finance.Blpapi.SessionOptions
import           Finance.Blpapi.Types            as BT

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans             (liftIO)
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Lazy                  as T
import qualified Data.Text.Lazy.IO               as TIO
import           Options.Applicative

data RefDataResponse = RefDataResponse {
    refSecurity :: Text,
    refFields   :: [(Text, String)] -- [name, value]
} deriving (Show)

notifyDone :: MVar () -> Event -> Blpapi ()
notifyDone m e
    | eventType e == EventTypeResponse = liftIO $! putMVar m ()
    | isEventSessionTerminated e = liftIO $! putMVar m ()
    | otherwise = return ()

defaultHandler :: MVar () -> Event -> Blpapi ()
defaultHandler m e = do
  liftIO $ printEvent e
  parseRefData e
  notifyDone m e

printEvent :: Event -> IO ()
printEvent e = TIO.putStrLn $ pretty (eventContent e)

parseRefData :: Event -> Blpapi ()
parseRefData e
 | eventType e == EventTypeResponse
     || eventType e == EventTypePartialResponse = do
            case parseRef (messageData (eventContent e)) of
                  Left err -> liftIO $ putStrLn err
                  Right lst -> liftIO $ print lst
            return ()
 | otherwise = return ()

parseRef :: Element -> Either String [RefDataResponse]
parseRef el = do
    e <- P.getElement "ReferenceDataResponse" el
    secDatas <- P.getElement "securityData" e >>= P.getArrayElements
    foldM (\acc it -> do
       secName <- P.getElement "security" it >>= P.getValue
       fieldDatas <- P.getElement "fieldData" it >>= P.getAllElements
       fieldValues <- foldM (\ acc2 (fn, it2) -> do
          value <- P.getValue it2
          Right ((fn, value):acc2)) [] fieldDatas
       return (RefDataResponse secName fieldValues:acc)) [] secDatas

createRefDataRequest :: Blpapi ()
createRefDataRequest = do
    ser <- openService "//blp/refdata" >>= throwOnError
    req <- createRequest ser "ReferenceDataRequest" >>= throwOnError
    formatRequest req $! do
        formatSubElement "returnEids" $
            setValue (BT.BlpBool True)
        formatSubElement "fields" $ do
            appendValue (BT.BlpString "ASK")
            appendValue (BT.BlpString "BID")
        formatSubElement "securities" $ do
            appendValue (BT.BlpString "IBM US Equity")
            appendValue (BT.BlpString "GOOG US Equity")
    e <- getElementFromRequest req
    liftIO $ prettyPrint e
    sendRequest req Nothing

data CmdOptions = CmdOptions {
  cmdIp   :: String,
  cmdPort :: Int
} deriving (Show)

setupBlpapi :: CmdOptions -> Blpapi ()
setupBlpapi c = do
  m <- liftIO newEmptyMVar
  createSession
     (defaultSessionOptions
          {serverAddresses = [ServerAddress (cmdIp c) (cmdPort c)]})
     (defaultHandler m) >>= throwOnError
  createRefDataRequest
  liftIO $! takeMVar m

cmdLineParser :: Parser CmdOptions
cmdLineParser = CmdOptions <$> ipParser <*> portParser
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
  runBlpapi $ setupBlpapi cmd
  putStrLn "About to Quit !!"
  return ()
