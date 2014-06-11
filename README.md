blpapi-hs
=========
[Bloomberg Open API] binding for [Haskell].

Find source code in the [Github repository].

[Bloomberg Open API]: http://openbloomberg.com/open-api
[Github repository]: https://github.com/bloomberg/blpapi-hs
[Haskell]: http://www.haskell.org/

Dependencies
------------
This module requires:

+ Linux, Microsoft Windows, or Apple Mac OS X (32 or 64-bit)
+ Haskell (ghc >= 7.4)
+ [BLPAPI C++ SDK] (>= v3.6)

[BLPAPI C++ SDK]: http://openbloomberg.com/open-api

Installation on Linux
---------------------
First, you will need to install the [BLPAPI C++ SDK]. Download the appropriate package
for your system, and unpack the downloaded file into a suitable directory. These
instructions assume you have unpacked the SDK into ```/opt/blpapi```, but if you have
chosen another directory, substitute its path in the instructions below.

After unpacking the C++ SDK, you will need to build this project on your system. The
```cabal``` tool provided with the Haskell installatin can be used to do that. The tool
will need to be told where the C++ SDK header files and libraries are located on your system.

```
$ cabal install --extra-include-dirs=/opt/blpapi/include --extra-lib-dirs=/opt/blpapi/Linux
```

[BLPAPI C++ SDK]: http://openbloomberg.com/open-api

Installation on Microsoft Windows
-----------------------------------------------------------------------------------
First, you will need to install the [BLPAPI C++ SDK]. Download the appropriate package
for your system, and unpack the downloaded file into a suitable directory. These
instructions assume you have unpacked the SDK into ```C:\blpapi```, but if you have
chosen another directory, substitute its path in the instructions below.

After unpacking the C++ SDK, you will need to build this project on your system. The
```cabal``` tool provided with the Haskell installatin can be used to do that. The tool
will need to be told where the C++ SDK header files and libraries are located on your system.

```
$ cabal install --extra-include-dirs=C:\blpapi\include --extra-lib-dirs=C:\blpapi\lib
```

[BLPAPI C++ SDK]: http://openbloomberg.com/open-api

Installation on Apple Mac OS X
------------------------------
First, you will need to install the [BLPAPI C++ SDK]. Download the appropriate package
for your system, and unpack the downloaded file into a suitable directory. These
instructions assume you have unpacked the SDK into ```/opt/blpapi```, but if you have
chosen another directory, substitute its path in the instructions below.

After unpacking the C++ SDK, you will need to build this project on your system. The
```cabal``` tool provided with the Haskell installatin can be used to do that. The tool
will need to be told where the C++ SDK header files and libraries are located on your system.

```
$ cabal install --extra-include-dirs=/opt/blpapi/include --extra-lib-dirs=/opt/blpapi/Darwin
```

[BLPAPI C++ SDK]: http://openbloomberg.com/open-api

Building the examples
---------------------
This project contains a few example programs in addition to the API wrapper library; they can
be built by adding the ```-fbuildExamples``` argument to the ```cabal``` command when building
the project.

Usage
-----
The module design closely follows the BLPAPI SDK design with very slight
modifications to make it more 'Haskell-like'. The SDK developer guide should
serve as the main guide to understand the API.

Full examples contained in the `examples` directory demonstrate how to
use most SDK functionality.  Full descriptions of all availabe requests,
responses, and options are contained within the BLPAPI API
[Developer Guide](http://www.openbloomberg.com/files/2012/03/blpapi-developers-guide.pdf).


### Setting up a Session
    setupBlpapi :: Blpapi ()
    setupBlpapi c = do
      createSession
        (defaultSessionOptions {serverAddresses = [ServerAddress "127.0.0.1" 8194]})
        (defaultHandler m)

### Create a Ref Data Request
    createRefDataRequest :: Blpapi ()
    createRefDataRequest = do
        ser <- openService "//blp/refdata"
        req <- createRequest ser "ReferenceDataRequest"
        formatRequest req $! do
            formatSubElement "fields" $ do
                appendValue (BT.BlpString "ASK")
                appendValue (BT.BlpString "BID")
            formatSubElement "securities" $ do
                appendValue (BT.BlpString "IBM US Equity")
                appendValue (BT.BlpString "GOOG US Equity")
        e <- getElementFromRequest req
        liftIO $ prettyPrint e
        sendRequest req Nothing

### Set up a Subscription
    getSubscriptionList :: [Subscription]
    getSubscriptionList =
      [Subscription (TopicString "IBM US Equity" ["BID", "ASK"]) Nothing Nothing]

    setupSubscription :: Identity -> Blpapi ()
    setupSubscription iden = do
      openService "//blp/mktdata"
      subscribeWithIdentity getSubscriptionList iden

### Process a Response
    defaultHandler :: Event -> Blpapi ()
    defaultHandler m e = do
      liftIO $ printEvent e
      parseRefData e

License
-------
MIT-style license. See license text in [LICENSE](https://github.com/bloomberg/blpapi-hs/blob/master/LICENSE).

