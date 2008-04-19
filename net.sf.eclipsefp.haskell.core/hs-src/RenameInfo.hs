module RenameInfo where

import Marshal
import SrcLoc

data RenameInfo =
     RenameInfo { file :: FilePath 
     	        , position :: SrcLoc
     		, replacement :: String 
     	        }
     	      
instance UnMarshal RenameInfo where     	       
    unmarshal_partial xs0 =
        let ( f, xs1 ) = unmarshal_partial xs0
            ( p, xs2 ) = unmarshal_partial xs1
            ( r, xs3 ) = unmarshal_partial xs2
        in  ( RenameInfo { file = unmarshal f
        	   , position = unmarshal p
        	   , replacement = unmarshal r 
        	   }
             , xs3
             )
             
    