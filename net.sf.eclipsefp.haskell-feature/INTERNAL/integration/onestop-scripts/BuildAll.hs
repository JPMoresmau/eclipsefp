{-# OPTIONS_GHC -Wall #-}
module Main( main ) where

import Control.Monad( filterM, sequence )
import Data.List( isPrefixOf )
import Data.Version( Version(..), parseVersion, showVersion )
import System( getArgs )
import System.Cmd( system )
import System.Console.GetOpt( ArgDescr(..), getOpt, OptDescr(..), ArgOrder( RequireOrder ) )
import System.Directory
import System.FilePath( takeExtension )
import Text.ParserCombinators.ReadP

main :: IO ()
main = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  dirs <- getDirs
  sequence $ map ( workOn $ optVersion opts ) dirs 
  putStrLn "Done."


-- helping functions
--------------------

workOn :: Version -> FilePath -> IO ()
workOn version dir = do
  -- prepare
  --
  parent <- getCurrentDirectory
  putStrLn $ "Working on dir: " ++ dir
  setCurrentDirectory dir
  --
  -- extract
  --
  members <- getDirectoryContents "."
  let zips = filter (\n -> (takeExtension n) == ".zip") members
  sequence $ map (system.((++) "unzip ")) zips 
  let targs = filter (\n -> (takeExtension n) == ".gz") members
  let cmds = map ((++) "tar xfz ") targs
  sequence $ map system cmds
  putStrLn "... Extraction completed"
  createDirectory "eclipse"
  system "mv features eclipse/"
  system "mv plugins eclipse/"
  --
  -- create build
  --
  let filename = "../onestop_net.sf.eclipsefp.haskell_" ++ showVersion version ++ "-" ++ dir ++ ".tar.gz"
  system $ unwords ["tar", "cfz", filename, "eclipse/features/*", "eclipse/plugins/*"]
  putStrLn $ "... Archive created: " ++ filename
  --
  -- postpare
  -- 
  sequence $ map removeDirectoryRecursive ["eclipse"]
  setCurrentDirectory parent
  putStrLn "... Cleanup done"
  
getDirs :: IO [ FilePath ]
getDirs = do
  members <- getDirectoryContents "."
  result <- filterM doesDirectoryExist members
  return $ filter (not.isPrefixOf ".") result


-- command line options stuff
-----------------------------

data Options = Options  {
    optVersion :: Version
  }

defaultOptions :: Options
defaultOptions = Options {
    optVersion = undefined
  }

options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (ReqArg getVersion "VERSION") "Version to build"
  ]

getVersion :: String -> Options -> IO Options
getVersion arg opt = return opt { optVersion = fst $ last $ readP_to_S parseVersion arg }
