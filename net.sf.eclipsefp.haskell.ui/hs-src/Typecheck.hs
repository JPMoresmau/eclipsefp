
module Typecheck (
  typecheckFiles, GhcModule(..), getSysLibDir, getSession,
) where

-- import Haddock.GHC.Utils(moduleString)
-- import Haddock.Types(GhcModule(..))
-- import Haddock.Exception(throwE)

-- import Data.Maybe
-- import Control.Monad
-- import GHC
-- import Digraph
-- import BasicTypes
-- import SrcLoc

-- import Data.List

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
typecheckFiles :: Session -> [FilePath] -> ErrorT String IO [GhcModule]
typecheckFiles session files = do

  -- load all argument files

  targets <- mapM (\f -> liftIO $ guessTarget f Nothing) files
  liftIO $ setTargets session targets

  flag <- liftIO $ load session LoadAllTargets
  when (failed flag)
    (throwError $ "Failed to load all needed modules")

  modgraph <- liftIO $ getModuleGraph session

  let mods = concatMap flattenSCC $ topSortModuleGraph False modgraph Nothing
      getModFile = fromJust . ml_hs_file . ms_location
      mods'= [ (ms_mod modsum, ms_hspp_opts modsum, getModFile modsum) |
               modsum <- mods ]

      -- typecheck the argument modules

  ghcMods <- forM mods' $ \(mod, flags, file) -> do
    mbMod <- liftIO $ checkModule session (moduleName mod) False
    case mbMod of
      Just (CheckedModule a (Just b) (Just c) (Just d) _)
        -> return $ mkGhcModule (mod, file, (a,b,c,d)) flags
      _ -> throwError $ "Failed to check module: " ++ moduleString mod

  return ghcMods

-- | Dig out what we want from the typechecker output
mkGhcModule :: CheckedMod -> DynFlags -> GhcModule
mkGhcModule (mod, file, checkedMod) dynflags = GhcModule {
  ghcModule         = mod,
  ghcFilename       = file,
  ghcMbDocOpts      = mbOpts,
  ghcHaddockModInfo = info,
  ghcMbDoc          = mbDoc,
  ghcGroup          = group,
  ghcMbExports      = mbExports,
  ghcExportedNames  = modInfoExports modInfo,
  ghcNamesInScope   = fromJust $ modInfoTopLevelScope modInfo, 
  ghcInstances      = modInfoInstances modInfo
}
  where
    HsModule _ _ _ _ _ mbOpts _ _      = unLoc parsed
    (group, _, mbExports, mbDoc, info) = renamed
    (parsed, renamed, _, modInfo)      = checkedMod


-- | This structure holds the module information we get from GHC's
-- type checking phase
data GhcModule = GhcModule {
   ghcModule         :: Module,
   ghcFilename       :: FilePath,
   ghcMbDocOpts      :: Maybe String,
   ghcHaddockModInfo :: HaddockModInfo Name,
   ghcMbDoc          :: Maybe (HsDoc Name),
   ghcGroup          :: HsGroup Name,
   ghcMbExports      :: Maybe [LIE Name],
   ghcExportedNames  :: [Name],
   ghcNamesInScope   :: [Name],
   ghcInstances      :: [Instance]
}



moduleString :: Module -> String
moduleString = moduleNameString . moduleName 


getSysLibDir :: IO FilePath
getSysLibDir = do
    (_, out, _, pid) <- runInteractiveProcess "ghc" ["--print-libdir"] Nothing Nothing
    libDir <- hGetLine out
    let libDir2 = if ord (last libDir) == 13   -- Windows!
                    then init libDir
                    else libDir
    waitForProcess pid
    return (FilePath.normalise libDir2)

getSession :: FilePath -> IO Session
getSession srcRoot =
   do libDir <- getSysLibDir
      session <- newSession (Just libDir)
      -- we must do that otherwise GHC crashes
      -- getSessionDynFlags session >>= setSessionDynFlags session
      dynFlags0 <- getSessionDynFlags session
      -- TODO: extract package list from Cabal file
      let options = ["-i:" ++ srcRoot, "-package=ghc"]
      (dynFlags1,str) <- parseDynamicFlags dynFlags0 options
      print str
      setSessionDynFlags session dynFlags1
      return session
