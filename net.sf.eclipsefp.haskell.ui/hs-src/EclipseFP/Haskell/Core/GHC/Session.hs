module EclipseFP.Haskell.Core.GHC.Session (getSession, loadModules) where

import BasicTypes(failed)
import Digraph(flattenSCC)
import GHC(Module(moduleName), moduleNameString, TypecheckedSource, RenamedSource, ParsedSource,
           HsModule(..), ModuleInfo, ModSummary(ms_mod, ms_location, ms_hspp_opts),
           CheckedModule(CheckedModule), LoadHowMuch(LoadAllTargets), DynFlags,
           setTargets, addTarget, guessTarget, load, checkModule, topSortModuleGraph, getModuleGraph,
           modInfoTopLevelScope, modInfoExports, modInfoInstances, ModLocation(ml_hs_file),
           Name, LIE, Instance, HsGroup, HsDoc, HaddockModInfo, 
           Session, newSession, getSessionDynFlags, setSessionDynFlags, parseDynamicFlags, )
import SrcLoc(unLoc)

import Control.Monad.Error (ErrorT, throwError, liftIO, )
import Control.Monad(Monad(return), mapM, forM, when)

import System.FilePath as FilePath
import System.Process (runInteractiveProcess, waitForProcess, )
import System.IO (hGetLine, )

import Data.Char (ord)
import Data.List((++), concatMap)
import Data.Maybe(Maybe(..), fromJust)


-- TODO: make it handle cleanup
loadModules :: Session -> [FilePath] -> ErrorT String IO ()
loadModules session files = do
  -- load all argument files
  targets <- mapM (\f -> liftIO $ guessTarget f Nothing) files
--  liftIO $ setTargets session targets
  liftIO $ mapM (addTarget session) targets
  flag <- liftIO $ load session LoadAllTargets
  when (failed flag)
    (throwError $ "Failed to load all needed modules")

getSession :: FilePath -> FilePath -> IO Session
getSession ghcLibDir srcRoot =
   do session <- newSession (Just ghcLibDir)
      -- we must do that otherwise GHC crashes
      -- getSessionDynFlags session >>= setSessionDynFlags session
      dynFlags0 <- getSessionDynFlags session
      -- TODO: extract package list from Cabal file
      let options = ["-i:" ++ srcRoot, "-package=ghc"]
      (dynFlags1,str) <- parseDynamicFlags dynFlags0 options
      print str
      setSessionDynFlags session dynFlags1
      return session
