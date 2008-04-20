module CodeFoldingGHC where

import Typecheck (getSession, typecheckFiles, )
import Control.Monad.Error (runErrorT, ErrorT, liftIO, )

import Bag (bagToList, )
import qualified Data.List as List
import Data.Maybe (mapMaybe, )

import System.FilePath ((</>))

-- import PprCore (pprType)
import TypeRep (pprType, )
import qualified Var
import qualified Pretty
import HsExpr (pprFunBind, )
import HsBinds (LHsBinds, HsBind(..), ppr_monobind, pprPrag, fun_infix, fun_matches, )
import Outputable
         (Outputable, OutputableBndr, PprStyle,
          showSDocUnqual, BindingSite(LetBind),
          (<+>), ppr, pprBndr, nest, vcat, hcat, text, dcolon, )
import SrcLoc (Located(L), spans, unLoc, SrcSpan, )
import qualified SrcLoc


data FoldingRegion = FoldingRegion Int Int   -- start and end line of region


computeFoldingRegions :: FilePath -> FilePath -> IO [String]
computeFoldingRegions srcRoot fileName =
   fmap (either (const []) (marshal . findFoldingRegions)) $
   runErrorT $
   do session <- liftIO $ getSession srcRoot
      ghcmods <- typecheckFiles session [srcRoot </> fileName]
      -- TODO: Is the current module always the last one?
      let (modul, moduleFileName, (_,_,typeCheckedMod,_)) = last ghcmods
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
