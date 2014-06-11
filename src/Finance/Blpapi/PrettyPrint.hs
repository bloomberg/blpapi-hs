{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Finance.Blpapi.PrettyPrint
Description : Printing Utility for Blpapi types
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

A pretty printer for 'Element' and 'Message'.
-}
module Finance.Blpapi.PrettyPrint (
  Config(..),
  defConfig,
  BlpPretty(..),
  prettyPrint,
  prettyPrint'
  ) where

import           Data.List              (intersperse)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (mappend, mconcat, mempty)
import qualified Data.Text              as T
import           Data.Text.Lazy         (Text)
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import qualified Data.Text.Lazy.IO      as TIO
import           Finance.Blpapi.Event
data PState = PState { pstIndent :: Int
                     , pstLevel  :: Int
                     }

-- | Configuration that can be passed to pretty print routines
data Config = Config {
    confIndent :: Int
      -- ^ Indentation spaces per level of nesting
    }

-- | Class which determines what BLPAPI types can be pretty printed
class BlpPretty a where
  pretty' :: Config -> a -> Text
  pretty :: a -> Text
  pretty = pretty' defConfig

instance BlpPretty Element where
  pretty' = encodePrettyElement'

instance BlpPretty Message where
  pretty' config m = pretty' config (messageData m)

-- |Pretty print an 'Finance.Blpapi.Element' or 'Finance.Blpapi.Message'
--with the default configuration
prettyPrint :: (BlpPretty a) => a -> IO ()
prettyPrint = TIO.putStrLn . pretty

-- |Pretty print an 'Finance.Blpapi.Element' or 'Finance.Blpapi.Message'
--with the provided configuration
prettyPrint' :: (BlpPretty a) => Config -> a -> IO ()
prettyPrint' c = TIO.putStrLn . pretty' c

-- | The default configuration: indent by four spaces per level of nesting
defConfig :: Config
defConfig = Config { confIndent = 4 }

pStateFromConfig :: Config -> PState
pStateFromConfig (Config ind) = PState ind 0

encodePrettyElement' :: Config -> Element -> Text
encodePrettyElement' c
      = toLazyText . fromValue (pStateFromConfig c)

fromValue :: PState -> Element -> Builder
fromValue st = go
  where
    go (ElementArray v)  = fromCompound st ("[","]") fromValue v
    go (ElementSequence m) = fromCompound st ("{","}") fromPair $ getPair m
    go (ElementChoice m) = fromCompound st ("{","}") fromPair $ getPair m
    go v = fromText $ fromMaybe "" $ blpConvert v
    getPair = Map.toList

fromCompound :: PState
             -> (Builder, Builder)
             -> (PState -> a -> Builder)
             -> [a]
             -> Builder
fromCompound st (delimL,delimR) fromItem items = mconcat
    [ delimL
    , if null items then mempty
        else "\n" <> items' <> "\n" <> fromIndent st
    , delimR
    ]
  where
    items' = mconcat . intersperse ",\n" $
                map (\item -> fromIndent st' <> fromItem st' item)
                    items
    st' = st { pstLevel = pstLevel st + 1 }

fromPair :: PState -> (T.Text, ElementWithDefinition) -> Builder
fromPair st (name, o)
    = fromText name <> ": " <> fromValue st (elementWithDefinitionContent o)

fromIndent :: PState -> Builder
fromIndent (PState ind lvl) = mconcat $ replicate (ind * lvl) " "

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
infixr 6 <>
