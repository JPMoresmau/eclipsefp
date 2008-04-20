module HooverType where

import Typecheck (getSession, typecheckFiles, )
import Control.Monad.Error (ErrorT, liftIO, runErrorT, )
import OutputableAdd

import Bag (bagToList, )
import qualified Data.List as List
import Data.Maybe (mapMaybe, catMaybes)

import System.FilePath ((</>))

import qualified GHC
import HscTypes		( implicitTyThings )
import qualified NameSet
import qualified PprTyThing

-- import PprCore (pprType)
import TypeRep (pprType, )
import qualified Var
import HsExpr (pprFunBind, )
import HsBinds (LHsBinds, HsBind(..), ppr_monobind, pprPrag, fun_infix, fun_matches, )
import Outputable (Outputable, SDoc,showSDoc, showSDocForUser, showSDocUnqual, OutputableBndr, BindingSite(LetBind),
          (<+>), ($$), empty, ppr, pprBndr, nest, vcat, hcat, text, dcolon, )
import SrcLoc (Located(L), spans, unLoc, )


type SrcLoc = (Int,Int)

mshow :: (Outputable a) =>  a -> String
mshow = showSDoc . ppr


getInfo :: FilePath -> FilePath -> String -> ErrorT String IO String
getInfo srcRoot fileName name =
    do session <- liftIO $ getSession srcRoot
       typecheckFiles session [fileName] 
       dflags  <- liftIO $ GHC.getSessionDynFlags session
       let pefas = True 
           -- False whether to show for alls: @dopt Opt_PrintExplicitForalls dflags@
       infoThing pefas session name
  where
    infoThing pefas session str = liftIO $ do
	names     <- GHC.parseName session str
	mb_stuffs <- mapM (GHC.getInfo session) names
	let filtered = filterOutChildren (\(t,_f,_i) -> t) (catMaybes mb_stuffs)
	unqual <- GHC.getPrintUnqual session
	return $ (showSDocForUser unqual $
     		                  vcat (List.intersperse (text "") $
		                        map (pprInfo pefas) filtered))


  -- Filter out names whose parent is also there Good
  -- example is '[]', which is both a type and data
  -- constructor in the same type
filterOutChildren :: (a -> GHC.TyThing) -> [a] -> [a]
filterOutChildren get_thing xs 
  = [x | x <- xs, not (GHC.getName (get_thing x) `NameSet.elemNameSet` implicits)]
  where
    implicits = NameSet.mkNameSet [GHC.getName t | x <- xs, t <- implicitTyThings (get_thing x)]

pprInfo :: PprTyThing.PrintExplicitForalls -> (GHC.TyThing, GHC.Fixity, [GHC.Instance]) -> SDoc
pprInfo pefas (thing, fixity, insts)
  =  PprTyThing.pprTyThingInContextLoc pefas thing
  $$ show_fixity fixity
  $$ vcat (map GHC.pprInstance insts)
  where
    show_fixity fix 
	| fix == GHC.defaultFixity = empty
	| otherwise		   = ppr fix <+> ppr (GHC.getName thing)


{-
Example call:
Control.Monad.Error.runErrorT $ getTypeSignature "/home/georg/src/Haskell/eclipse/eclipsefp2/net.sf.eclipsefp.haskell.ui/hs-src" "HooverType.hs" (25,3) >>= Control.Monad.Trans.liftIO . putStrLn

let srcRoot = "/home/georg/src/Haskell/eclipse/eclipsefp2/net.sf.eclipsefp.haskell.ui/hs-src"
let fileName = "Example.hs"
let srcLoc = (6,9)
runErrorT $ getInfo srcRoot fileName >>= putStrLn


session <- getSession srcRoot
Right ghcmods <-  runErrorT $ typecheckFiles session [srcRoot </> fileName]
-- should be lines
putStrLn $ mshow $ map (\(_,_,x) ->  x) $ ghcmods

-}
