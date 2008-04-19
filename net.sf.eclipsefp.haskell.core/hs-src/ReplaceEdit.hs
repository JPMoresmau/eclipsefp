{-# language OverlappingInstances #-}

module ReplaceEdit where

import Marshal

import Prelude hiding ( length )
import qualified Prelude

data ReplaceEdit = ReplaceEdit
    { file :: FilePath
    , offset :: Int
    , length :: Int
    , replacement :: String
    }
    deriving Show
    
instance Marshal ReplaceEdit where
    marshal r = marshal ( file r )
    	    ++ marshal ( offset r )
    	    ++ marshal ( length r )
    	    ++ marshal ( replacement r )
    	    
instance UnMarshal ReplaceEdit where
    unmarshal_partial xs0 = 
        let ( f, xs1 ) = unmarshal_partial xs0
            ( o, xs2 ) = unmarshal_partial xs1
            ( l, xs3 ) = unmarshal_partial xs2
            ( r, xs4 ) = unmarshal_partial xs3
        in  ( ReplaceEdit { file = f, offset = o, length = l, replacement = r }, xs4 )
        