-- Copyright (c) 2007 by Leif Frenzel <himself@leiffrenzel.de>
-- All rights reserved.
--
-- This is just a bridge to the code that does the real work, which was taken
-- from http://hackage.haskell.org/cgi-bin/hackage-scripts/package/pointfree-1.0.1
-- and is (c) by Thomas Jaeger. See the file LICENSE for license info. 

module MakePointFree where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

import Plugin.Pl.Common
import Plugin.Pl.Optimize
import Plugin.Pl.Parser
import Plugin.Pl.Transform

resource = plugin {
  pluginMain = performMakePointFree
}

performMakePointFree :: [String] -> IO [String]
performMakePointFree [arg] = return [makePointFree arg]
performMakePointFree _     = error "Bogus selection"

makePointFree :: String -> String
makePointFree input = case parsePF input of
  Right d -> show $ last $ mapTopLevel' optimize $ mapTopLevel transform d
  Left msg -> error msg