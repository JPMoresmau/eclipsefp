
module Cohatoe.Data( CohatoeData, marshal, module Cohatoe.Data.MarkerDesc ) where

import Cohatoe.Data.MarkerDesc

class CohatoeData a where
  marshal :: a -> [String]
  unmarshal :: [String] -> Maybe a
  
instance CohatoeData MarkerDesc where
  marshal md = [
      fileName md, show $ line md, message md, 
      show $ charStart md, show $ charEnd md, show $ severity md
    ]
  unmarshal []      = Nothing
  unmarshal (str:_) = Just $ read str
  