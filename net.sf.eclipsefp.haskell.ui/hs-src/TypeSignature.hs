module TypeSignature where

import Typecheck (getSession, typecheckFiles, )
import Control.Monad.Error (ErrorT, liftIO, )

import Bag (bagToList, )
import qualified Data.List as List
import Data.Maybe (mapMaybe, )

import System.FilePath ((</>))

import HsExpr (pprFunBind, )
import HsBinds (LHsBinds, HsBind(..), ppr_monobind, pprPrag, fun_infix, fun_matches, )
import Outputable (showSDocUnqual, OutputableBndr, BindingSite(LetBind), (<+>), ppr, pprBndr, nest, vcat, text, )
import SrcLoc (Located(L), spans, unLoc, )


type SrcLoc = (Int,Int)

getTypeSignature :: FilePath -> FilePath -> SrcLoc -> ErrorT String IO String
getTypeSignature srcRoot fileName srcLoc =
   do session <- liftIO $ getSession srcRoot
      ghcmods <- typecheckFiles session [srcRoot </> fileName]
      -- TODO: Is the current module always the last one?
      let (modul, moduleFileName, (_,_,typeCheckedMod,_)) = last ghcmods
      -- return (moduleFileName)
      return (findTypeSignature typeCheckedMod srcLoc)

findTypeSignature :: (OutputableBndr id) =>
   LHsBinds id ->   -- Bag (Located (HsBinds.HsBind id)) ->
   (Int, Int) ->
   String
findTypeSignature typeCheckedMod srcLoc =
   concatMap (\ (AbsBinds _ _ exports _) -> showSDocUnqual $
       vcat [pprBndr LetBind x | (_,x,_,_) <- exports]) $
--   concatMap (\ (AbsBinds _ _ exps _) -> showSDocUnqual $ vcat $ map ppr_exp exps) $
--   map (\ (AbsBinds _ _ _ _) -> 'a') $
--   map (\ (VarBind _ _) -> 'a') $
--   map (\ (PatBind _ _ _ _) -> 'a') $
--   map (\ (FunBind _ _ _ _ _ _) -> 'a') $
--   concatMap (showSDocUnqual . (\f -> pprFunBind (unLoc undefined) (fun_infix f) (fun_matches f))) $
--   concatMap (showSDocUnqual . (\f -> pprFunBind (unLoc "bla") (fun_infix f, fun_matches f))) $
--   concatMap (showSDocUnqual . ppr_monobind) $
   mapMaybe
      (\(L srcSpan bind) ->
         if spans srcSpan srcLoc
           then Just bind
           else Nothing) $
   bagToList typeCheckedMod
{-
  where
    ppr_exp (tvs, gbl, lcl, prags)
	= vcat [ppr gbl <+>  ppr tvs <+> ppr lcl,
	  	nest 2 (vcat (map (pprPrag gbl) prags))]
-}
