#!/bin/bash
export PATH=$PATH:/usr/local/bin
SRC=../../net.sf.eclipsefp.haskell.ui/hs-src
GHCI_CMD="ghci -i$SRC -package HUnit"
echo "Test.HUnit.runTestTT tests" | $GHCI_CMD CodeFolding_Test.hs | grep Cases
