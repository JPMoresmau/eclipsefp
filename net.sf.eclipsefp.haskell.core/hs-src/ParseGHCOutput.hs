-- Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
-- 
module ParseGHCOutput where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API
import Cohatoe.Data( marshal )

import GHCOutputParser( parseBuffer )

resource :: Interface
resource = plugin {
  pluginMain = performParseGHCOutput
}

performParseGHCOutput :: [String] -> IO [String]
performParseGHCOutput [] = return []
performParseGHCOutput (str:_) = return $ concatMap marshal (parseBuffer str)
