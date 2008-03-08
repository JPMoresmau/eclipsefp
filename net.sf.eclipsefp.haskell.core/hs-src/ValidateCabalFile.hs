-- Copyright (c) 2007-2008 by Leif Frenzel <himself@leiffrenzel.de>
-- All rights reserved.

module ValidateCabalFile where

import Data.Maybe( fromMaybe )

import Distribution.PackageDescription( parsePackageDescription )
import Distribution.InstalledPackageInfo( ParseResult(..) )
import Distribution.ParseUtils( locatedErrorMsg )

import Cohatoe.API
import Cohatoe.Data

resource :: Interface
resource = plugin {
  pluginMain = performPluginMain
}

performPluginMain :: [String] -> IO [String]
performPluginMain [] = return []
performPluginMain (cabalFile:_) = do
  descs <- validateCabalFile cabalFile
  return $ concatMap marshal descs

validateCabalFile :: FilePath -> IO [MarkerDesc]
validateCabalFile fpath = do
  str <- readFile fpath
  case parsePackageDescription str of
    ParseFailed err -> do
      let (lineNo, msg) = locatedErrorMsg err
      let ln = fromMaybe (-1) lineNo
      return [ MarkerDesc fpath ln msg unspecified unspecified Error ]
    ParseOk warns _ -> return $ map (mkWarning fpath) warns
  