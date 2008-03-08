#!/bin/bash
export PATH=$PATH:/usr/local/bin
SRC=../../net.sf.eclipsefp.haskell.core/hs-src
GHCI_CMD="ghci -i$SRC -package HUnit"
echo "Test.HUnit.runTestTT tests" | $GHCI_CMD ValidateCabalFile_Test.hs | grep Cases
