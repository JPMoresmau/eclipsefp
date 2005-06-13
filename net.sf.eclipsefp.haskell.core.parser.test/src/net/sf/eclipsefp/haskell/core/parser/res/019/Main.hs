
module Main () where

bla :: Bool -> Int -> Int
bla True 0 = 1
bla False 0 = 2
bla _ n = n + 2
