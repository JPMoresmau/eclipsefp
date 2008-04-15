{-# OPTIONS -fglasgow-exts #-}
-- | code folding ( ansatz, J. Waldmann )
-- version one: using Language.Haskell machinery, NOT GHC API
-- that means: parsing, but no import chasing, type checking etc.
module CodeFolding where

import qualified Language.Haskell.Syntax as S
import qualified Language.Haskell.Parser as P

import qualified Data.Tree as T

import Data.Maybe
import System.IO

import Cohatoe.API

resource :: Interface
resource = plugin {
  pluginMain = performCodeFolding
}

performCodeFolding :: [String] -> IO [String]
performCodeFolding [] = return []
performCodeFolding (content:_) = return $ marshal $ computeFoldingRegions content

marshal :: [FoldingRegion] -> [String]
marshal = concatMap mfr where
  mfr (NoFoldingRegion)         = []
  mfr (FoldingRegion start end) = [show start, show end]

-- | see http://leiffrenzel.de/eclipse/wiki/doku.php?id=editorcodefolding
data FoldingRegion 
     = FoldingRegion Int Int -- ^ start and end line region
     | NoFoldingRegion
    deriving ( Eq, Show )

computeFoldingRegions :: String -> [FoldingRegion]
computeFoldingRegions buffer = 
    let mm = P.parseModuleWithMode ( P.ParseMode { P.parseFilename = "input" } ) buffer
    in  case mm of
	P.ParseFailed loc msg -> error msg
        P.ParseOk m -> 
	    let ls = lines buffer
	        t = rangeInfo ( length ls ) $ Node m
	    in  filter isFoldingRegion $ map fst $ T.flatten t

isFoldingRegion :: FoldingRegion -> Bool
isFoldingRegion x = case x of
    FoldingRegion {} -> True
    _ -> False



--------------------------------------------------------------------------------------------

-- | uniform representation of AST nodes
class Show n => NodeC n  where
    -- | source location ( possibly )
    location :: n -> Maybe S.SrcLoc
    
    -- | children, in order
    children :: n -> [ Node ]
    children _ = []

data Node = forall a . NodeC a => Node a 

instance Show Node where
    show ( Node n ) = head $ words $ show n

instance NodeC Node where
    location ( Node n ) = location n
    children ( Node n ) = children n

instance NodeC S.HsModule where
    location ( S.HsModule p _ _ _ _ ) = Just p
    children ( S.HsModule _ name mexports imports decls ) 
        =  []
	++ ( map Node $ imports )
	++ ( map Node $ decls )

instance NodeC S.HsExportSpec where
    location x = Nothing
    
instance NodeC S.HsImportDecl where
    location = Just . S.importLoc

instance NodeC S.HsDecl where
    location x = case x of
        S.HsTypeDecl p _ _ _ -> Just p
	S.HsDataDecl p _ _ _ _ _ -> Just p
	S.HsInfixDecl p _ _ _ -> Just p
	S.HsNewTypeDecl p _ _ _ _ _  -> Just p	
	S.HsClassDecl p _ _ _ _ -> Just p 
	S.HsInstDecl p _ _ _ _ -> Just p
	S.HsDefaultDecl p _ -> Just p
	S.HsTypeSig p _ _ -> Just p
	S.HsFunBind _ -> Nothing
	S.HsPatBind p _ _ _ -> Just p
	S.HsForeignImport p _ _ _ _ _ -> Just p
	S.HsForeignExport p _ _ _ _ -> Just p
    children x = case x of
	S.HsDataDecl p _ _ _ cons quals -> 
	    map Node cons -- FIXME: quals do not have srcloc?
	S.HsNewTypeDecl p _ _ _ con quals  -> 
	    [ Node con ] -- FIXME: quals do not have srcloc?
	S.HsClassDecl p _ _ _ decls -> 
	    map Node decls
	S.HsInstDecl p _ _ _ decls -> 
	    map Node decls
	S.HsFunBind matches -> 
	    map Node matches
	S.HsPatBind p _ _ decls ->
	    map Node decls
	_ -> []


instance NodeC S.HsConDecl where
    location x = case x of
        S.HsConDecl p _ _ -> Just p
	S.HsRecDecl p _ _ -> Just p


instance NodeC S.HsMatch where
    location x = case x of
        S.HsMatch p _ _ _ _ -> Just p


--------------------------------------------------------------------------------------------

-- | given the line number of the next item (or the end-of-file),
-- annotate each node by its range
rangeInfo :: Int -> Node -> T.Tree (  FoldingRegion , Node )
rangeInfo end n = 
    let ( _, [ n' ] ) = rangeInfos end [n]
    in  n'

rangeInfos :: Int -> [  Node ] -> ( Int, [ T.Tree (  FoldingRegion , Node ) ] )
rangeInfos end [] = ( end, [] )
rangeInfos end ( n : ns ) = 
    let ( end', ns' ) = rangeInfos end ns
        ( _ , sub ) = rangeInfos end' $ children n
	( start, n' ) = case location n of
		  Nothing -> ( end', T.Node ( NoFoldingRegion , n ) sub )
		  Just p  -> let here = S.srcLine p 
			     in ( here, T.Node ( FoldingRegion here ( end' - 1) , n ) sub )
    in  ( start, n' : ns' )


