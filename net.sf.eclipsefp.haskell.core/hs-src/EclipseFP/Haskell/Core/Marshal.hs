{-# language TypeSynonymInstances, OverlappingInstances, FlexibleInstances, UndecidableInstances #-}

module EclipseFP.Haskell.Core.Marshal where

-- | from Haskel land to Java land
class Marshal a where 
    marshal :: a -> [ String ]

-- | from Java land to Haskell land
class UnMarshal a where 
    unmarshal_partial :: [ String ] -> ( a , [ String ] )
    
-- | complete unmarshalling
unmarshal css = let ( x, [] ) = unmarshal_partial css in x

instance  Marshal Int where
    marshal x = [ show x ]

instance UnMarshal Int where
    unmarshal_partial (x : xs) = ( read x , xs ) 

instance Marshal String where
    marshal s = [ s ] 
    
instance UnMarshal String where
    unmarshal_partial (x : xs ) = ( x, xs )
    
    
    