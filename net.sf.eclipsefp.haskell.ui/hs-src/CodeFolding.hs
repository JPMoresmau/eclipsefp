module CodeFolding where

import Control.Monad( liftM )
import Control.Monad.Error (runErrorT, ErrorT, liftIO, )

import Bag (bagToList, )
import qualified Data.List as List
import Data.Maybe (mapMaybe, )

import System.FilePath ((</>))

import qualified Var
import HsBinds (LHsBinds, )
import SrcLoc (Located(L), )
import qualified SrcLoc

import Cohatoe.API

import Typecheck (getSession, typecheckFiles, )

resource :: Interface
resource = plugin {
  pluginMain = performCodeFolding
}

-- start and end line of region
data FoldingRegion = FoldingRegion Int Int deriving (Eq, Show)

performCodeFolding :: [String] -> IO [String]
performCodeFolding (libDir:srcRoot:file:_) = liftM marshal (computeFoldingRegions libDir srcRoot file)
performCodeFolding _ = return []

computeFoldingRegions :: FilePath -> FilePath -> FilePath -> IO [FoldingRegion]
computeFoldingRegions ghcLibDir srcRoot fileName =
   fmap (either (const []) (findFoldingRegions)) $
   runErrorT $
   do session <- liftIO $ getSession ghcLibDir srcRoot
      ghcmods <- typecheckFiles session [srcRoot </> fileName]
      -- TODO: Is the current module always the last one?
      let (_, _, (_,_,typeCheckedMod,_)) = last ghcmods
      return typeCheckedMod

marshal :: [FoldingRegion] -> [String]
marshal =
   concatMap (\(FoldingRegion line column) -> [show line, show column])

findFoldingRegions :: -- (OutputableBndr id) =>
   LHsBinds Var.Var ->   -- Bag (Located (HsBinds.HsBind id)) ->
   [FoldingRegion]
findFoldingRegions typeCheckedMod =
   mapMaybe
      (\(L srcSpan _) ->
          if SrcLoc.isGoodSrcSpan srcSpan &&
             not (SrcLoc.isOneLineSpan srcSpan)
            then Just (FoldingRegion
                    (SrcLoc.srcSpanStartLine srcSpan)
                    (SrcLoc.srcSpanEndLine srcSpan))
            else Nothing) $
   bagToList typeCheckedMod

{-
      (\(L srcSpan _) ->
         case srcSpan of
            SrcSpanMultiLine _ _ _ _ _ ->
                 Just $ FoldingRegion
                    (srcSpanSLine srcSpan) (srcSpanELine srcSpan)    
            _ -> Nothing) $
-}
