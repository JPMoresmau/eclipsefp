module Main where

data Eq a => Set a = NilSet | ConsSet a (Set a)

data Temp = Cold | Hot