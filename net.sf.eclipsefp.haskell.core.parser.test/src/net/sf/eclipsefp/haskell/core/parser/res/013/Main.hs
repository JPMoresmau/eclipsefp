module Main where

instance Visible Bool where
  toString True  = "True"
  toString False = "False"
  size _ = 1
  