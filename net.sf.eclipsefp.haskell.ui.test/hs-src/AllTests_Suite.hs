-- Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 
-- | collects all HUnit test cases; this is for the convenience of running
--   them all at once, and also so that we can build a single executable to
--   run with hpc.

import Test.HUnit

import CodeFolding_Test

main = runTestTT $ TestList [ 
    CodeFolding_Test.tests 
  ]
