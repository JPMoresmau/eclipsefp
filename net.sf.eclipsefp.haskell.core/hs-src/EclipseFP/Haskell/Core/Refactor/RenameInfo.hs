--  FIXME: this should be  ... Refactor.Rename.Info

module EclipseFP.Haskell.Core.Refactor.RenameInfo where

import EclipseFP.Haskell.Core.Marshal
import EclipseFP.Haskell.Core.SrcLoc

data RenameInfo =
     RenameInfo { file :: FilePath 
     	        , position :: SrcLoc
     		, replacement :: String 
     	        }
     	    deriving Show
     	    
instance Marshal RenameInfo where
    marshal i = marshal ( file i )
    	++ marshal ( position i )
    	++ marshal ( replacement i )
     	      
instance UnMarshal RenameInfo where     	       
    unmarshal_partial xs0 =
        let ( f, xs1 ) = unmarshal_partial xs0
            ( p, xs2 ) = unmarshal_partial xs1
            ( r, xs3 ) = unmarshal_partial xs2
        in  ( RenameInfo { file = f
        	   , position = p
        	   , replacement =  r 
        	   }
             , xs3
             )
             
    