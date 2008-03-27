-- Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 
module GHCOutputParser_Test( tests ) where

import Control.Monad( liftM )
import Test.HUnit

import Cohatoe.Data.MarkerDesc

import GHCOutputParser( parseBuffer )

tests :: Test
tests = TestList [
    testCase "001" [MarkerDesc "Main.hs" 3 " Not in scope: `fac'" 25 27 Error],
    testCase "002" [MarkerDesc "Main.hs" 3 " Not in scope: `f'" 25 unspecified Error],
    testCase "003" [MarkerDesc "Main.hs" 3 " Not in scope: `Compiling'" 25 33 Error],
    testCase "004" [
        MarkerDesc "Main.hs" 4 " Not in scope: `fac'" 26 28 Error,
        MarkerDesc "Main.hs" 4 " Not in scope: `fib'" 32 34 Error
      ],
    testCase "005" [MarkerDesc "Main.hs" 4 " Not in scope: `fac'" 25 27 Error],
    testCase "006" [MarkerDesc "Main.hs" 6 " Not in scope: `fib'" 25 27 Error],
    testCase "007" [MarkerDesc "Main.hs" 6 " Not in scope: `fib'" 25 27 Error],
    testCase "008" [MarkerDesc "Main.hs" 4 " Not in scope: `fac'" 28 30 Error],
    testCase "009" [MarkerDesc "Main.hs" 4 "    Failed to load interface for `Factorial':" 0 15 Error],
    testCase "010" [], -- hm, what could we possibly report back in this case?
    testCase "011" [MarkerDesc "Main.hs" 4 "    Expecting a function type, but found `IO a'\r" 0 unspecified Error],
    testCase "012" [
        MarkerDesc "Main.hs" 4 " Definition but no type signature for `main'\r" 0 unspecified Warning,
        MarkerDesc "Main.hs" 4 " Defaulting the following constraint(s) to type `Integer'\r" 13 14 Warning
      ],
    testCase "013" [
        MarkerDesc "Main.hs" 3 " Definition but no type signature for `main'" 0 23 Warning,
        MarkerDesc "Main.hs" 3 " Defaulting the following constraint(s) to type `Integer'" 22 23 Warning,
        MarkerDesc "Main.hs" 5 " Definition but no type signature for `ladida'" 0 18 Warning
      ],
    testCase "014" [MarkerDesc "GHCOutputParser.hs" 4 " Imported from `Control.Monad' but not used: `liftM2'" 29 34 Warning]
  ]
  

-- helping functions
--------------------

testCase :: String -> [MarkerDesc] -> Test
testCase name expected = TestCase $ do
  valRes <- liftM parseBuffer (readFile $ "res/ghc_output_parser/" ++ name)
  assertEqual ("Case " ++ name) expected valRes
  