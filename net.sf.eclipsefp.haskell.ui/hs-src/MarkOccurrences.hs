module MarkOccurrences where 

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded
import Cohatoe.API
import Data.Maybe( fromMaybe )
import FindIdentifier( findIdentifier )

resource = plugin {
  pluginMain = execute
}

execute :: [String] -> IO [String]
execute args = return $ fromMaybe [] (findIdentifier (head args) (line, colum)) where
  (line, colum) = (read $ args !! 1, read $ args !! 2)
