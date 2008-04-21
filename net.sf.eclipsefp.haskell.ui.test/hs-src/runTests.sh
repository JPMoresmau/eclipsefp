#!/bin/bash
export PATH=$PATH:/usr/local/bin
SRC_UI=../../net.sf.eclipsefp.haskell.ui/hs-src
SRC_CORE=../../net.sf.eclipsefp.haskell.core/hs-src
GHCI_CMD="ghci -i$SRC_UI -i$SRC_CORE -package HUnit -package ghc"
echo "Test.HUnit.runTestTT tests" | $GHCI_CMD CodeFolding_Test.hs | grep Cases
