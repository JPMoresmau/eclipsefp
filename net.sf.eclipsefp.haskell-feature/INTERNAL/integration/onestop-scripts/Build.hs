
module Main( main ) where

import Control.Monad( liftM )
import System( getArgs )
import System.Cmd( system )


type Version = String
type MaterialPattern = String
data Cmd =   Unzip MaterialPattern 
           | UnzipToEclipse MaterialPattern 
           | CreateBuild Version
           | Clean
  deriving (Show, Read)

class Command a where
  createCmdLine :: a -> String

instance Command Cmd where
  createCmdLine (Unzip matPat) = unwords ["unzip", matPat]
  createCmdLine (UnzipToEclipse matPat) 
    = unwords ["unzip", "-d", "eclipse/", matPat]
  createCmdLine (CreateBuild ver) 
    = unwords ["tar", "cvfz", "onestop_net.sf.eclipsefp.haskell_" ++ ver ++ "-win32.tar.gz eclipse/*"]
  createCmdLine Clean = unwords ["rm", "-r", "eclipse/"]

main :: IO ()
main = do 
  cmds <- liftM read (readFile "commands.txt") :: IO [Cmd]
  sequence $ map (system.createCmdLine) cmds
  putStrLn "Done."

