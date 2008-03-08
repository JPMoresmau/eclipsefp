-- Copyright (c) 2006-2008 by Leif Frenzel - see http://leiffrenzel.de
-- This code is made available under the terms of the Eclipse Public 
-- License, version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
module Cohatoe.Data.MarkerDesc (
    MarkerDesc(..), 
    Severity(..),
    unspecified,
    mkWarning
  ) where

data MarkerDesc = MarkerDesc {
  fileName :: String,
  line :: Int,
  message :: String,
  charStart :: Int,
  charEnd :: Int,
  severity :: Severity
} deriving (Eq, Show, Read)

data Severity = Error | Warning
  deriving (Eq, Show, Read)

unspecified :: Int
unspecified = -1

-- convenience function for making a warning with file name and message,
-- locations unspecified
mkWarning :: FilePath -> String -> MarkerDesc
mkWarning fp msg = MarkerDesc fp unspecified msg unspecified unspecified Warning