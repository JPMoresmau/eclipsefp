module EclipseFP.Haskell.Core.GHC.TypeCheck (
  CheckedMod, typeCheckFiles
) where


import BasicTypes(failed)
import Digraph(flattenSCC)
import GHC(Module(moduleName), moduleNameString, TypecheckedSource, RenamedSource, ParsedSource,
           HsModule(..), ModuleInfo, ModSummary(ms_mod, ms_location, ms_hspp_opts),
           CheckedModule(CheckedModule), LoadHowMuch(LoadAllTargets), DynFlags,
           setTargets, guessTarget, load, checkModule, topSortModuleGraph, getModuleGraph,
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


type CheckedMod = (Module, FilePath, FullyCheckedMod)


type FullyCheckedMod = (ParsedSource, 
                        RenamedSource,
                        TypecheckedSource,
                        ModuleInfo)


-- TODO: make it handle cleanup
typeCheckFiles :: Session -> [FilePath] -> ErrorT String IO [CheckedMod]
typeCheckFiles session files = do

  -- load all argument files

  targets <- mapM (\f -> liftIO $ guessTarget f Nothing) files
  liftIO $ setTargets session targets

  flag <- liftIO $ load session LoadAllTargets
  when (failed flag)
    (throwError $ "Failed to load all needed modules")

  modgraph <- liftIO $ getModuleGraph session

  let mods = concatMap flattenSCC $ topSortModuleGraph False modgraph Nothing
      getModFile = fromJust . ml_hs_file . ms_location
      mods'= [ (ms_mod modsum, getModFile modsum) |
               modsum <- mods ]

      -- typecheck the argument modules

  forM mods' $ \(mod, file) -> do
    mbMod <- liftIO $ checkModule session (moduleName mod) False
    case mbMod of
      Just (CheckedModule a (Just b) (Just c) (Just d) _)
        -> return (mod, file, (a,b,c,d))
      _ -> throwError $ "Failed to check module: " ++ moduleString mod



moduleString :: Module -> String
moduleString = moduleNameString . moduleName 

