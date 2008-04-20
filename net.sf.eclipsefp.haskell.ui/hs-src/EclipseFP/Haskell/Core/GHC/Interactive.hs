module EclipseFP.Haskell.Core.GHC.Interactive where

import SrcLoc()    -- Instances only
import GHC(GHC.Fixity, GHC.defaultFixity, GHC.NamedThing(GHC.getName),
           GHC.Instance, GHC.TyThing, GHC.getSessionDynFlags, GHC.getPrintUnqual,
           GHC.getInfo, GHC.parseName, GHC.pprInstance)
import HsBinds()    -- Instances only
import HsExpr()    -- Instances only
import HscTypes(implicitTyThings)
import NameSet(NameSet.elemNameSet, NameSet.mkNameSet)
import Outputable(Outputable(..), SDoc, vcat, text, ($$), (<+>), empty,
                  showSDocForUser)
import PprTyThing(PprTyThing.PrintExplicitForalls,
                  PprTyThing.pprTyThingInContextLoc)
import TypeRep()    -- Instances only
import Var()    -- Instances only
import Control.Monad.Error(Control.Monad.Trans.MonadIO, ErrorT, liftIO)
import qualified Data.List as List
import Data.Maybe(catMaybes)

import EclipseFP.Haskell.Core.GHC.Session(getSession)

getInfo :: FilePath -> FilePath -> String -> ErrorT String IO String
getInfo srcRoot fileName name =
    do session <- liftIO $ getSession srcRoot
--       typecheckFiles session [fileName] 
--        load modules! 
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
