module Main where

infix 5 `op1`
infixr 1 `op2`

infixl 5 `op2`

infix 5 `op1`, `op2`
infixl 0 `op1`, +, `opx`
