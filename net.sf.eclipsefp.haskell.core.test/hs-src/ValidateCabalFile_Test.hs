-- Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 

module ValidateCabalFile_Test where

import Test.HUnit

import Cohatoe.Data.MarkerDesc
import ValidateCabalFile( validateCabalFile )

-- test cases
-------------

tests = TestList [ 
    testCase "001" [ mkWarning "res/001" "No library or executable specified" ],
    testCase "002" [ 
        mkWarning "res/002" "A package using section syntax should require\n\"Cabal-Version: >= 1.2\" or equivalent.", 
        mkWarning "res/002" "Ignoring trailing declarations.",
        mkWarning "res/002" "Unknown section type: totally ignoring..." 
      ],
    testCase "003" [ mkWarning "res/003" "No library or executable specified" ],
    testCase "004" []
  ]


-- helping functions
--------------------

testCase :: String -> [MarkerDesc] -> Test
testCase name expected = TestCase $ do
  valRes <- validateCabalFile $ "res/" ++ name
  assertEqual ("Case " ++ name) expected valRes
  