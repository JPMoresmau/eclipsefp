-- Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public License,
-- version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
module ManipulateCabalFile where

-- We must import Cohatoe.API and implement resource so that this code
-- can be dynamically loaded as plugin.
import Cohatoe.API

import Data.Maybe( fromJust )
import Data.Version( parseVersion, showVersion )
import Distribution.Package( PackageIdentifier(..) ) 
import Distribution.PackageDescription( 
  parsePackageDescription,
  showPackageDescription,
  PackageDescription(..),
  GenericPackageDescription(..),
  Library(..),
  Executable(..),
  BuildInfo(..) )
import Distribution.InstalledPackageInfo( ParseResult(..) )
import Distribution.ParseUtils( locatedErrorMsg )
import Text.ParserCombinators.ReadP( readP_to_S )


resource :: Interface
resource = plugin {
  pluginMain = performManipulateCabaFile
}

-- convention: first arg is the buffer, second the command, third a param
-- convention: if we return an error, the first result elem is empty, the second
--             contains the error message
performManipulateCabaFile :: [String] -> IO [String]
performManipulateCabaFile args = do
  let (buffer, cmd, param) = readArgs args
  return $ case parsePackageDescription buffer of
    ParseFailed err -> [ "", snd $ locatedErrorMsg err ]
    ParseOk _ (GenericPackageDescription pd _ _ _) -> case cmd of
      -- accessors
      "GET_NAME"            -> [pkgName $ package pd]
      "GET_VERSION"         -> [showVersion $ pkgVersion $ package pd]
      "GET_COPYRIGHT"       -> [copyright pd]
      "GET_LICENSE"         -> [show $ license pd]
      "GET_LICENSE_FILE"    -> [licenseFile pd]
      "GET_DESCRIPTION"     -> [description pd]
      "GET_SYNOPSIS"        -> [synopsis pd]
      "GET_HOMEPAGE"        -> [homepage pd]
      "GET_CATEGORY"        -> [category pd]
      "GET_AUTHOR"          -> [author pd]
      "GET_MAINTAINER"      -> [maintainer pd]
      "GET_ALL_SOURCE_DIRS" -> collectSourceDirs pd
      -- mutators
      -- TODO lf shouldn't use fromJust
      "SET_NAME"         -> setName pd param
      "SET_VERSION"      -> setVersion pd param
      "SET_COPYRIGHT"    -> update (pd { copyright = fromJust param }) param
      "SET_LICENSE"      -> setLicense pd param
      "SET_LICENSE_FILE" -> update (pd { licenseFile = fromJust param }) param
      "SET_DESCRIPTION"  -> update (pd { description = fromJust param }) param
      "SET_SYNOPSIS"     -> update (pd { synopsis = fromJust param }) param
      "SET_HOMEPAGE"     -> update (pd { homepage = fromJust param }) param
      "SET_CATEGORY"     -> update (pd { category = fromJust param }) param
      "SET_AUTHOR"       -> update (pd { author = fromJust param }) param
      "SET_MAINTAINER"   -> update (pd { maintainer = fromJust param }) param
      _                  -> ["", "Unknown call"]

setName :: PackageDescription -> Maybe String -> [String]
setName _ Nothing = ["", "No name given!"]
setName pd (Just newName) = [showPackageDescription mkNewPkgIdentifier] where
  mkNewPkgIdentifier = (pd { package = (package pd) { pkgName = newName } } )

setVersion :: PackageDescription -> Maybe String -> [String]
setVersion _ Nothing = ["", "No version given!"]
setVersion pd (Just newVersion) 
  = [showPackageDescription $ pd { package = mkNewPkgIdentifier }] where
    mkNewPkgIdentifier = case reverse $ readP_to_S parseVersion newVersion of
      []       -> package pd -- revert to the old value if parse failed
      (pres:_) -> (package pd) { pkgVersion = fst pres }

setLicense :: PackageDescription -> Maybe String -> [String]
setLicense _ Nothing = ["", "No license given!"]
setLicense pd (Just param) 
  = [showPackageDescription $ pd { license = parseLicense }] where
    parseLicense = case reverse $ reads param of
      []       -> license pd -- revert to the old value if parse failed
      (pres:_) -> fst pres

update :: PackageDescription -> Maybe String -> [String]
update _ Nothing    = ["", "No param!"]
update newPkgDesc _ = [showPackageDescription $ newPkgDesc]

collectSourceDirs :: PackageDescription -> [String]
collectSourceDirs pd = (collectFromLib $ library pd) ++ (concatMap collectFromExe (executables pd)) where
  collectFromLib Nothing = []
  collectFromLib (Just (Library _ bi)) = hsSourceDirs bi
  collectFromExe (Executable _ _ bi)   = hsSourceDirs bi

readArgs :: [String] -> (String, String, Maybe String)
readArgs (s1:s2:s3:_) = (s1, s2, Just s3)
readArgs (s1:s2:_) = (s1, s2, Nothing)
readArgs _         = ("", "", Nothing)

