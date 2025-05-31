#!/bin/sh

cabal clean
cabal configure
cabal build
cabal test

