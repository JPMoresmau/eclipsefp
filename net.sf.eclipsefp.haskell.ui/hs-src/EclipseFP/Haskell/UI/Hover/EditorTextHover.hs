module EclipseFP.Haskell.UI.Hover.EditorTextHover where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

import EclipseFP.Haskell.Core.Marshal
import EclipseFP.Haskell.Core.SrcLoc

import qualified TypeSignature as T

import Control.Monad.Error

resource = plugin {
  pluginMain = performEditorTextHover
}


--  FIXME: should go in Hover.Info module
data HoverInfo = 
     HoverInfo { ghcLibDir :: FilePath
               , cabal :: FilePath
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
        Left msg -> error $ "Problem was: " ++ msg -- FIXME
	Right res -> return $ marshal res


getTypeSignature :: HoverInfo -> ErrorT String IO String
getTypeSignature hi = 
    T.getTypeSignature ( ghcLibDir hi ) ( cabal hi ) ( srcmod hi ) ( line $ srcloc hi, column $ srcloc hi )


instance UnMarshal HoverInfo where
    unmarshal_partial xs0 = 
        let ( d, xs1 ) = unmarshal_partial xs0
            ( c, xs2 ) = unmarshal_partial xs1
	    ( m, xs3 ) = unmarshal_partial xs2
	    ( l, xs4 ) = unmarshal_partial xs3
	in  ( HoverInfo { ghcLibDir = d, cabal = c, srcmod = m, srcloc = l } , xs3 )
             
