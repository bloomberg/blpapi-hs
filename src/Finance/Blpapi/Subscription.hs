{-|
Module      : Finance.Blpapi.Subscription
Description : The subscription type
Copyright   : Bloomberg Finance L.P.
License     : MIT
Maintainer  : agupta182@bloomberg.net
Stability   : experimental
Portability : *nix, windows

This module provides a structure to hold the data used 'subscribe',
'resubscribe', and 'unsubscribe' methods.  This structure comprises a list
in which each list entry contains two primary fields: a 'CorrelationId'
associated with the subscription, and a string, called a *subscription*
*string*, describing the data to be delivered as a part of the
subscription.

The simplest form of a subscription string is a *fully* *qualified*
subscription string, which has the following structure:

@
\/\/blp\/mktdata\/ticker\/IBM US Equity?fields=BID,ASK&interval=2
\\-----------\/\\------\/\\-----------\/\\------------------------\/
      |          |         |                  |
   Service    Prefix   Instrument           Suffix
@

Such a fully-qualified string is composed of:
Service Identifier: a string matching the expression
 '^\/\/[-_.a-zA-Z0-9]+\/[-_.a-zA-Z0-9]+$', e.g. \/\/blp\/mktdata.  See
 'blpapi_abstractsession' for further details.

Prefix:
A string matching the expression '\/([-_.a-zA-Z0-9]+\/)?', often
used as a symbology identifier.  Common examples include '/ticker/' and
'/cusip/'.  Not all services make use of prefices.  Note than an "empty"
topic prefix consists of the string "/", so the topic prefix always
separates the service string from the instrument string.

Instrument:
A non-empty string that does not contain the character '?' (i.e. a string
matching '[^?]+') e.g. 'IBM US Equity', or 'SPX Index'. The service,
prefix, and instrument together uniquely identify a source for
subscription data.

Suffix:
a suffix contains a question mark followed by a list of options which can
affect the content delivery.  The format of these options is service
specific, but they generally follow URI query string conventions:
a sequence of 'key=value' pairs separated by "&" characters.  Further,
many common services follow the convention that the value given for the
'fields' key is formatted as a comma-separated list of field names. BLPAPI
provides several convenience functions to assist in formatting
subscription strings for services that make use of these conventions.

Subscription strings need not be fully qualified: BLPAPI allows the service
and prefix to be omitted from subscription strings, and automatically
qualifies these strings using information stored in a 'Session' object.

The subscription strings passed to 'BLPAPI' are automatically qualified if
the service identifier is missing (i.e. if the subscription string does not
start with '//'). The subscription parameters (i.e. the optional part after
instrument identifier) are never modified.

The rules for qualifying the subscription string are:

1. If the subscription string begins with '//' then it is assumed to be
  a fully qualified subscription string including service identifier, prefix,
  and instrument.  In this case the string will not be modified and session
  options defaults have no affect on the subscription.

2. If the subscription string begins with a '/' and the second character is
  not '/', then the string is assumed to begin with the topic prefix, but no
  service identifier. In this case the string is qualified by prepending the
  'SessionOptions' 'defaultSubscriptionService' to the specified string.

3. If the subscription string does not begin with a '/' it is assumed to begin
  with an instrument identifier.  In this case the string is qualified by
  prepending the 'SessionOptions' 'defaultSubscriptionService' followed by
  'SessionOptions' 'defaultTopicPrefix' to the specified string. If the
  'defaultTopicPrefix' is empty or null, then the prefix used is '/'.
  Otherwise (in the case of a nontrivial prefix) if the separator '/' is
  not specified at the beginning or the end of the 'defaultTopicPrefix',
  then it will be added.
-}
module Finance.Blpapi.Subscription where

import           Data.List
import           Finance.Blpapi.CorrelationId
import           Finance.Blpapi.Identity

data TopicString
  = QualifiedTopicString {_topicString :: String}
  | TopicString { _topic  :: String,
                  _fields :: [String]}
  | TopicStringWithOptions { _topic   :: String,
                             _fields  :: [String],
                             _options :: [String]}
  deriving (Show, Eq)

data Subscription = Subscription {
      subscriptonString       :: TopicString,
      subscriptionCorrelation :: Maybe CorrelationId,
      subscriptionIdentity    :: Maybe Identity
    } deriving (Show, Eq)

-- | Convert any 'TopicString' to a Fully Qualifiedtopicstring as mentioned
-- in the module documentation
convertToQualifiedString :: TopicString -> TopicString
convertToQualifiedString (TopicString topic fields) =
    convertToQualifiedString $ TopicStringWithOptions topic fields []
convertToQualifiedString (TopicStringWithOptions topic fields options) =
  QualifiedTopicString $ topic ++ fieldsList fields ++ optionList options
  where
    fieldsList [] = []
    fieldsList _ = "?fields=" ++ intercalate "," fields
    optionList [] = []
    optionList _ = "&" ++ intercalate "&" options

