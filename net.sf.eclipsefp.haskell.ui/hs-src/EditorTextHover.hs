module EditorTextHover where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

resource = plugin {
  pluginMain = performEditorTextHover
}

-- covention: we get cabal file, src file, location as pair of Int
performEditorTextHover :: [String] -> IO [String]
performEditorTextHover args = undefined
-- performEditorTextHover args = do
--    res < runErrorT getTypeSignature 
--getTypeSignature :: FilePath -> FilePath -> SrcLoc -> ErrorT String IO String
