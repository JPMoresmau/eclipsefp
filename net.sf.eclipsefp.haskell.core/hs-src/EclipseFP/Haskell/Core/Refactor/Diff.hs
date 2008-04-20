module EclipseFP.Haskell.Core.Refactor.Diff where

import Prelude hiding ( Either (..))

import Data.Array
import Data.List ( group )

data Direction = Left | Right | Both | Stop
    deriving ( Eq, Ord, Show )

edit :: Eq a => [a] -> [a] -> [ Edit a ]
edit xs ys = let a = table xs ys
                 ((0,0),(p,q)) = bounds a
             in  reverse $ traverse a (p,q) ys

--  NOTE: quadratic cost
table :: Eq a => [a] -> [a] -> Array (Int,Int) (Int, Direction)
table xs ys = 
    let bnd = ((0,0),(length xs, length ys))
        a   = array bnd $ do
	    (p,q) <- range bnd
	    return ( (p,q)
                   , if p == 0 || q == 0 
		     then ( 0, Stop )
		     else maximum [ ( fst ( a!(p-1,q) ), Right )
				  , ( fst ( a!(p,q-1) ), Left )
			 	  , if xs !! (p-1) == ys !! (q-1) 
				    then ( 1 + fst ( a!(p-1,q-1) ), Both ) 
				    else ( 0, Both )
				  ] 
		   )
    in  a

data Edit a = Delete Int | Insert [a] | Keep Int
   deriving ( Eq, Ord, Show )

traverse a (p,q) ys  = 
    if p == 0 then if q == 0 then [] else  [ Insert ( take q ys ) ]
    else if q == 0 then [ Delete p ]
    else let ( h, d ) = a ! (p,q) 
         in case d of
	       Left -> Insert [ ys !! ( q - 1 ) ] : traverse a (p,q-1) ys
	       Right -> Delete 1  : traverse a (p-1, q) ys
	       Both -> Keep 1 : traverse a (p-1,q-1) ys


compress :: Eq a => [ Edit a ] -> [ Edit a ]
compress [] = []
compress [x] = [x]
compress (x:xs) = 
    let y : ys = compress xs
        top = case (x,y) of
	    ( Insert x, Insert y ) -> [ Insert (x ++ y)  ]
	    ( Delete a, Delete b ) -> [ Delete ( a + b) ]
	    ( Keep   a, Keep   b ) -> [ Keep   ( a + b ) ]
            _ -> [ x , y ]
    in  top ++ ys

test = compress $ edit "haskell" "hackathon"
