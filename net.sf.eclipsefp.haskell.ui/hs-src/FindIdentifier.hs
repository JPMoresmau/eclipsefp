module FindIdentifier( findIdentifier ) where

import Control.Monad( liftM )
import Data.List( sort )
import Data.Maybe
import Data.Generics
import Language.Haskell.Parser
import Language.Haskell.Syntax

-- I'm using a hacked version of Language.Haskell (package haskell-src) which
-- gives some more source locations, in particular for HsIdent and HsSymbol.
-- Then I'm just traversing the AST (with SYB) and look at all the identifiers
-- that are visited. If the passed source location is within that identifier,
-- we're successful and return the identifier.

-- (TODO problem: the parser generates a 'Main' module declaration and a
-- 'main' function if no explicit module declaration is given, all this
-- at source loc (1, 1) - so we end up with multiple elements at (1, 1), 
-- and these must be filtered out)

-- We expect the content of a Haskell module and a source code position in 
-- terms of line/column in that content.
findIdentifier :: String -> (Int, Int) -> Maybe [String]
findIdentifier []      _   = Nothing
findIdentifier content pos =
  if pos < (1, 1) 
    then Nothing
    else do
      hsModule <- fromParseResult $ parseModule content
      identifier <- getIdentifierForPos hsModule pos
      return $ collectIdentifierOccs identifier hsModule 


-- internal functions
---------------------

fromParseResult (ParseOk hm) = Just hm
fromParseResult _ = Nothing

getIdentifierForPos :: HsModule -> (Int, Int) -> Maybe String
getIdentifierForPos hsModule pos = do 
  let occs = sort $ collectOccurrences hsModule
  occ <- find occs pos
  fromOcc occ pos
  
-- SYB - luckily, the boilerplate has already been scrapped for us :-)
--       So we can just generically walk the tree and collect occurrences
collectOccurrences :: HsModule -> [(SrcLoc, String)]
collectOccurrences = everything (++) ([] `mkQ` getOcc) where
  getOcc (HsIdent loc str)  = [(loc, str)]
  getOcc (HsSymbol loc str) = [(loc, str)]

collectIdentifierOccs :: String -> HsModule -> [String]
collectIdentifierOccs ident = everything (++) ([] `mkQ` appendOcc) where 
  appendOcc (HsIdent loc str)  = if( str == ident ) then encode loc else [] 
  appendOcc (HsSymbol loc str) = if( str == ident ) then encode loc else []
  encode (SrcLoc _ line col) = [show line, show col, (show $ length ident)]

find :: [(SrcLoc, String)] -> (Int, Int) -> Maybe (SrcLoc, String)
find []   _   = Nothing
find locs pos = getLast $ locsBefore locs pos where
  getLast [] = Nothing
  getLast locs = Just $ last locs

locsBefore locs pos = filter (before pos) locs where
  before :: (Int, Int) -> (SrcLoc, String) -> Bool
  before (posL, posC) ( (SrcLoc _ line col), _ ) = 
    line < posL || ( line == posL && col <= posC )

-- we have the first identifier before the specified position, but still the
-- end of the identifier must be after the specified position (else the cursor
-- would in fact be after the identifier that we have found) 
fromOcc :: (SrcLoc, String) -> (Int, Int) -> Maybe String
fromOcc ((SrcLoc _ line col), ident) (posL, posC) = do
  if line /= posL || (col + length ident) < posC 
    then Nothing
    else Just ident
    