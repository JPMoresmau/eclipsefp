-- testing module with some export specifications
module Main (idf1, module Bla) where

import Bla

idf1 :: Int -> Int
idf1 n = n + 42