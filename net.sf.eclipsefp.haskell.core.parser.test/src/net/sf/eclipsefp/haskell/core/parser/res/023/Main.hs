-- lots of things that have source locations
module Main (main, 
             getTheAnswer, 
             module Haskell.Language.Syntax) where

import Haskell.Language.Syntax
import Bla
import Blubb

data Eq a => Set a =   NilSet 
                     | ConsSet a (Set a)

data Temp = Cold | Hot

idf2, idf3 :: Int
idf2 = 42

getTheAnswer, getItAgain :: Int -> Int
getTheAnswer n = 42

class Visible a where
  toString :: a -> String
  size :: a -> Int
  
instance Visible Bla where
  toString Bla = "Bla"
  size Bla = 42
  
infix 5 `op1`
infixr 1 `op2`

infixl 5 `op2`

infix 5 `op1`, `op2`
infixl 0 `op1`, +, `opx`

newtype Age = Age { unAge :: Int }

type Rec a = [Circ a]

default (Integer, Double)
