{-# language DisambiguateRecordFields #-}

module Rename where

import RenameInfo
import ReplaceEdit
import SrcLoc



-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

resource = plugin {
  pluginMain = performRename
}


     	        



-- TODO: Here's what we actually do when someone calls us.
performRename :: [String] -> IO [String]
performRename = fmap marshal . refactor . unmarshal 

unmarshal :: [ String ] -> RenameInfo
unmarshal = undefined

marshal :: [ ReplaceEdit ] -> [ String ]
marshal = undefined

refactor :: RenameInfo -> IO [ ReplaceEdit ]
refactor = undefined
