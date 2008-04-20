module EclipseFP.Haskell.Core.GHC.TypeCheck (
  CheckedMod, typeCheckFiles
) where

import BasicTypes(failed)
import Digraph(flattenSCC)
import GHC(TypecheckedSource, RenamedSource, ParsedSource, ModuleInfo,
           Module(moduleName), ModSummary(ms_location, ms_mod),
           CheckedModule(CheckedModule), LoadHowMuch(LoadAllTargets), Session, setTargets,
           guessTarget, load, checkModule, topSortModuleGraph, getModuleGraph,
           ModLocation(ml_hs_file), moduleNameString)
import Control.Monad.Error
          (MonadError, MonadIO, ErrorT, liftIO, throwError, )
import System.FilePath(FilePath, )
import Control.Monad(Monad(return), mapM, forM, when, )
import Data.Maybe (fromMaybe, )
import Data.List((++), concatMap, )


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
      getModFile = fromMaybe (error "file expected") . ml_hs_file . ms_location
      mods'= [ (ms_mod modsum, getModFile modsum) |
               modsum <- mods ]

  -- typecheck the argument modules
  forM mods' $ \(modu, file) -> do
    mbMod <- liftIO $ checkModule session (moduleName modu) False
    case mbMod of
      Just (CheckedModule a (Just b) (Just c) (Just d) _)
        -> return (modu, file, (a,b,c,d))
      _ -> throwError $ "Failed to check module: " ++ moduleString modu



moduleString :: Module -> String
moduleString = moduleNameString . moduleName 

