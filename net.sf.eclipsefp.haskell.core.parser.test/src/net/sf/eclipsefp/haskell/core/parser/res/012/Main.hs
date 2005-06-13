module Main where

class Eq a => Hash a where
  hash :: a -> Int

class Visible a where
  toString :: a -> String
  size :: a -> Int