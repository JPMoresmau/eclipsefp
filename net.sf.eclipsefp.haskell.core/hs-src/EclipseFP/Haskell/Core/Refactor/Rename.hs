{-# language DisambiguateRecordFields #-}

module EclipseFP.Haskell.Core.Refactor.Rename where

import EclipseFP.Haskell.Core.Marshal

import EclipseFP.Haskell.Core.Refactor.RenameInfo as I
import EclipseFP.Haskell.Core.Refactor.ReplaceEdit

import Network
import System.IO

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
    clientCmd [ "new", I.file ri ]
    return $ return $ ReplaceEdit
           { file = I.file ri
    	   , offset = 5
           , length = 2
           , replacement = "bar"
	   }
       
--  from HaRe/pfe_client
clientCmd :: [String] -> IO ()
clientCmd args = withSocketsDo $ do
  h <- connectTo "localhost" $ PortNumber 9000
  hSetBuffering h LineBuffering
  hPutStrLn stderr $ "(stderr) CLIENT SEND: "++unwords args
  putStrLn $ "(stdout) CLIENT SEND: "++unwords args
  hPutStrLn h $ unwords args
  hGetLine h  -- wait for acknowledgement
  return ()


 
    
    