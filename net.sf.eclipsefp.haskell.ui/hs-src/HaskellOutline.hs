module HaskellOutline where

import Cohatoe.API

resource :: Interface
resource = plugin {
  pluginMain = performHaskellOutline
}

-- TODO: Here's what we actually do when someone calls us.
performHaskellOutline :: [String] -> IO [String]
performHaskellOutline args = undefined

data TreeElement = TreeElement Int String String
