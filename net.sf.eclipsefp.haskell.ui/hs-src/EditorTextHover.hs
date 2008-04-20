module EditorTextHover where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

import Marshal
import SrcLoc

import qualified TypeSignature as T

import Control.Monad.Error

resource = plugin {
  pluginMain = performEditorTextHover
}

data HoverInfo = 
     HoverInfo { cabal :: FilePath
	       , srcmod   :: FilePath
	       , srcloc :: SrcLoc
               }


-- covention: we get cabal file, src file, location as pair of Int
performEditorTextHover :: [String] -> IO [String]
performEditorTextHover args = do
    let hi :: HoverInfo
	hi  = unmarshal args
    res <- runErrorT $ getTypeSignature hi
    case res of
        Left msg -> error msg -- FIXME
	Right res -> return $ marshal res


getTypeSignature :: HoverInfo -> ErrorT String IO String
getTypeSignature hi = 
    T.getTypeSignature ( cabal hi ) ( srcmod hi ) ( line $ srcloc hi, column $ srcloc hi )


instance UnMarshal HoverInfo where
    unmarshal_partial xs0 = 
        let ( c, xs1 ) = unmarshal_partial xs0
	    ( m, xs2 ) = unmarshal_partial xs1
	    ( l, xs3 ) = unmarshal_partial xs2
	in  ( HoverInfo { cabal = c, srcmod = m, srcloc = l } , xs3 )
             
