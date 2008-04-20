module OutputableAdd where

import Outputable

instance Outputable a, Outputable b, Outputable c, Outputable d, Outputable e => Outputable (a,b,c,d,e) where
    ppr (a,b,c,d,e) = parens (sep [ppr a <> comma, ppr b <> comma
                                  , ppr c <> comma, ppr d <> comma, ppr e])


{-
HooverType Outputable> let srcRoot = "/home/georg/src/Haskell/eclipse/eclipsefp2/net.sf.eclipsefp.haskell.ui/hs-src"
*HooverType Outputable> let fileName = "Example.hs"
*HooverType Outputable> let srcLoc = (6,9)
*HooverType Outputable> session <- getSession srcRoot
[]
*HooverType Outputable> Right ghcmods <-  runErrorT $ typecheckFiles session [srcRoot </> fileName]
*HooverType Outputable> -- should be lines
*HooverType Outputable> putStrLn $ mshow $ map (\(_,_,x) ->  x) $ ghcmods
[(module Example where
  test :: String -> [String]
  test = lines
  test1 :: Int -> Int
  test1 i = i + i,
  (Example.test :: GHC.Base.String -> [GHC.Base.String]
   Example.test = Data.List.lines
   Example.test1 :: GHC.Base.Int -> GHC.Base.Int
   Example.test1 i = i GHC.Num.+ i,
   [import Prelude],
   Nothing,
   Nothing,
   HaddockModInfo),
  <AbsBinds [] [] [Example.test1 <= [] test1]
     Example.test1 :: GHC.Base.Int -> GHC.Base.Int
     []
     { + = (GHC.Num.+) @ GHC.Base.Int $dNum
       $dNum = GHC.Num.$f6
       test1 i = i + i },
   AbsBinds [] [] [Example.test <= [] test]
     Example.test :: GHC.Base.String -> [GHC.Base.String]
     []
     { test = Data.List.lines }>,
  ModuleInfo)]
- }