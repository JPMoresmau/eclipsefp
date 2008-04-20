module EclipseFP.Haskell.Core.GHC.TypeCheck (
  CheckedMod, typeCheckFiles
) where

import qualified EclipseFP.Haskell.Core.GHC.Session as Session

import Digraph(flattenSCC)
import GHC(TypecheckedSource, RenamedSource, ParsedSource, ModuleInfo,
           Module(moduleName), ModSummary(ms_location, ms_mod),
           CheckedModule(CheckedModule), Session,
           checkModule, topSortModuleGraph, getModuleGraph,
           ModLocation(ml_hs_file), moduleNameString)

import Control.Monad.Error
          (MonadError, MonadIO, ErrorT, liftIO, throwError, )
import System.FilePath(FilePath, )
import Control.Monad(Monad(return), forM, )
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
  Session.loadModules session files

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

