{-# language DisambiguateRecordFields #-}

module Rename where

import Marshal

import RenameInfo
import ReplaceEdit

import Prelude hiding (length )


-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

resource = plugin {
  pluginMain = performRename
}


-- TODO: Here's what we actually do when someone calls us.
performRename :: [String] -> IO [String]
performRename input = do
    let ri = unmarshal input
    re <- refactor ri
    return $ concat $ map marshal re

refactor :: RenameInfo -> IO [ ReplaceEdit ]
refactor ri = do
    return $ return $ ReplaceEdit
           { file = RenameInfo.file ri
    	   , offset = 5
           , length = 2
           , replacement = "bar"
	   }
    
    
    