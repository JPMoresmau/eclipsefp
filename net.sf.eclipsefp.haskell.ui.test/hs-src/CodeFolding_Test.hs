-- Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 
module CodeFolding_Test( tests ) where

import Control.Monad( liftM )
import Test.HUnit

import CodeFolding( computeFoldingRegions, FoldingRegion(..) )

tests :: Test
tests = TestList [
    -- empty file
    testCase "001" [],
    -- only single-line stuff - no folding
    testCase "002" [],
    testCase "003" [FoldingRegion 5 7],
    testCase "004" [FoldingRegion 2 5],
    testCase "005" [FoldingRegion 4 6],
    testCase "006" [FoldingRegion 4 6],
    testCase "007" [FoldingRegion 1 2]
  ]
  

-- helping functions
--------------------

testCase :: String -> [FoldingRegion] -> Test
testCase name expected = TestCase $ do
  valRes <- liftM computeFoldingRegions (readFile $ "res/code_folding/" ++ name)
  assertEqual ("Case " ++ name) expected valRes
  