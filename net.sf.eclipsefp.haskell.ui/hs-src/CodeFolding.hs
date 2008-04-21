module CodeFolding where

import Typecheck (getSession, typecheckFiles, )
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

resource :: Interface
resource = plugin {
  pluginMain = performCodeFolding
}

data FoldingRegion = FoldingRegion Int Int   -- start and end line of region

performCodeFolding :: [String] -> IO [String]
performCodeFolding (srcRoot:file:_) = computeFoldingRegions srcRoot file
performCodeFolding _ = return []

computeFoldingRegions :: FilePath -> FilePath -> IO [String]
computeFoldingRegions srcRoot fileName =
   fmap (either (const []) (marshal . findFoldingRegions)) $
   runErrorT $
   do session <- liftIO $ getSession srcRoot
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
