cabal-install:
	cabal install --extra-include-dirs=/opt/blpapi/include --extra-lib-dirs=/opt/blpapi/Linux

stack-build:
	stack build --extra-include-dirs=/opt/blpapi/include --extra-lib-dirs=/opt/blpapi/Linux
