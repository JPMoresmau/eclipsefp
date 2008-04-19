module TypeSignature where

import Typecheck (getSession, typecheckFiles, ghcFilename, )
import Control.Monad.Error (ErrorT, liftIO, )

import qualified Data.List as List

import System.FilePath ((</>))

type SrcLoc = (Int,Int)

getTypeSignature :: FilePath -> FilePath -> SrcLoc -> ErrorT String IO String
getTypeSignature srcRoot fileName srcLoc =
   do session <- liftIO $ getSession srcRoot
      ghcmods <- typecheckFiles session [srcRoot </> fileName]
      -- TODO: Is the current module always the last one?
      let ghcmod = last ghcmods
      return (ghcFilename ghcmod)
