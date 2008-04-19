module Rename where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

resource = plugin {
  pluginMain = performRename
}

-- TODO: Here's what we actually do when someone calls us.
performRename :: [String] -> IO [String]
performRename args = undefined

