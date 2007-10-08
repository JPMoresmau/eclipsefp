{-# OPTIONS -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Parser
-- Copyright   :  (c) Simon Marlow, Sven Panne 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Haskell parser.
--
-----------------------------------------------------------------------------
module Language.Haskell.Parser (
		parseModule, parseModuleWithMode,
		ParseMode(..), defaultParseMode, ParseResult(..)) where

import Language.Haskell.Syntax
import Language.Haskell.ParseMonad
import Language.Haskell.Lexer
import Language.Haskell.ParseUtils
import Array
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif

-- parser produced by Happy Version 1.15

newtype HappyAbsSyn  = HappyAbsSyn (() -> ())
happyIn4 :: (HsModule) -> (HappyAbsSyn )
happyIn4 x = unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (HsModule)
happyOut4 x = unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (([HsImportDecl],[HsDecl])) -> (HappyAbsSyn )
happyIn5 x = unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (([HsImportDecl],[HsDecl]))
happyOut5 x = unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (([HsImportDecl],[HsDecl])) -> (HappyAbsSyn )
happyIn6 x = unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (([HsImportDecl],[HsDecl]))
happyOut6 x = unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (()) -> (HappyAbsSyn )
happyIn7 x = unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (())
happyOut7 x = unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (()) -> (HappyAbsSyn )
happyIn8 x = unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (())
happyOut8 x = unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyIn9 :: (Maybe [HsExportSpec]) -> (HappyAbsSyn )
happyIn9 x = unsafeCoerce# x
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> (Maybe [HsExportSpec])
happyOut9 x = unsafeCoerce# x
{-# INLINE happyOut9 #-}
happyIn10 :: ([HsExportSpec]) -> (HappyAbsSyn )
happyIn10 x = unsafeCoerce# x
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> ([HsExportSpec])
happyOut10 x = unsafeCoerce# x
{-# INLINE happyOut10 #-}
happyIn11 :: (()) -> (HappyAbsSyn )
happyIn11 x = unsafeCoerce# x
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> (())
happyOut11 x = unsafeCoerce# x
{-# INLINE happyOut11 #-}
happyIn12 :: ([HsExportSpec]) -> (HappyAbsSyn )
happyIn12 x = unsafeCoerce# x
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> ([HsExportSpec])
happyOut12 x = unsafeCoerce# x
{-# INLINE happyOut12 #-}
happyIn13 :: (HsExportSpec) -> (HappyAbsSyn )
happyIn13 x = unsafeCoerce# x
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> (HsExportSpec)
happyOut13 x = unsafeCoerce# x
{-# INLINE happyOut13 #-}
happyIn14 :: ([HsImportDecl]) -> (HappyAbsSyn )
happyIn14 x = unsafeCoerce# x
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> ([HsImportDecl])
happyOut14 x = unsafeCoerce# x
{-# INLINE happyOut14 #-}
happyIn15 :: (HsImportDecl) -> (HappyAbsSyn )
happyIn15 x = unsafeCoerce# x
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> (HsImportDecl)
happyOut15 x = unsafeCoerce# x
{-# INLINE happyOut15 #-}
happyIn16 :: (Bool) -> (HappyAbsSyn )
happyIn16 x = unsafeCoerce# x
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> (Bool)
happyOut16 x = unsafeCoerce# x
{-# INLINE happyOut16 #-}
happyIn17 :: (Maybe Module) -> (HappyAbsSyn )
happyIn17 x = unsafeCoerce# x
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> (Maybe Module)
happyOut17 x = unsafeCoerce# x
{-# INLINE happyOut17 #-}
happyIn18 :: (Maybe (Bool, [HsImportSpec])) -> (HappyAbsSyn )
happyIn18 x = unsafeCoerce# x
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> (Maybe (Bool, [HsImportSpec]))
happyOut18 x = unsafeCoerce# x
{-# INLINE happyOut18 #-}
happyIn19 :: ((Bool, [HsImportSpec])) -> (HappyAbsSyn )
happyIn19 x = unsafeCoerce# x
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> ((Bool, [HsImportSpec]))
happyOut19 x = unsafeCoerce# x
{-# INLINE happyOut19 #-}
happyIn20 :: (Bool) -> (HappyAbsSyn )
happyIn20 x = unsafeCoerce# x
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> (Bool)
happyOut20 x = unsafeCoerce# x
{-# INLINE happyOut20 #-}
happyIn21 :: ([HsImportSpec]) -> (HappyAbsSyn )
happyIn21 x = unsafeCoerce# x
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> ([HsImportSpec])
happyOut21 x = unsafeCoerce# x
{-# INLINE happyOut21 #-}
happyIn22 :: (HsImportSpec) -> (HappyAbsSyn )
happyIn22 x = unsafeCoerce# x
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> (HsImportSpec)
happyOut22 x = unsafeCoerce# x
{-# INLINE happyOut22 #-}
happyIn23 :: ([HsCName]) -> (HappyAbsSyn )
happyIn23 x = unsafeCoerce# x
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> ([HsCName])
happyOut23 x = unsafeCoerce# x
{-# INLINE happyOut23 #-}
happyIn24 :: (HsCName) -> (HappyAbsSyn )
happyIn24 x = unsafeCoerce# x
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> (HsCName)
happyOut24 x = unsafeCoerce# x
{-# INLINE happyOut24 #-}
happyIn25 :: (HsDecl) -> (HappyAbsSyn )
happyIn25 x = unsafeCoerce# x
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> (HsDecl)
happyOut25 x = unsafeCoerce# x
{-# INLINE happyOut25 #-}
happyIn26 :: (Int) -> (HappyAbsSyn )
happyIn26 x = unsafeCoerce# x
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> (Int)
happyOut26 x = unsafeCoerce# x
{-# INLINE happyOut26 #-}
happyIn27 :: (HsAssoc) -> (HappyAbsSyn )
happyIn27 x = unsafeCoerce# x
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> (HsAssoc)
happyOut27 x = unsafeCoerce# x
{-# INLINE happyOut27 #-}
happyIn28 :: ([HsOp]) -> (HappyAbsSyn )
happyIn28 x = unsafeCoerce# x
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> ([HsOp])
happyOut28 x = unsafeCoerce# x
{-# INLINE happyOut28 #-}
happyIn29 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn29 x = unsafeCoerce# x
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut29 x = unsafeCoerce# x
{-# INLINE happyOut29 #-}
happyIn30 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn30 x = unsafeCoerce# x
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut30 x = unsafeCoerce# x
{-# INLINE happyOut30 #-}
happyIn31 :: (HsDecl) -> (HappyAbsSyn )
happyIn31 x = unsafeCoerce# x
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> (HsDecl)
happyOut31 x = unsafeCoerce# x
{-# INLINE happyOut31 #-}
happyIn32 :: ([HsType]) -> (HappyAbsSyn )
happyIn32 x = unsafeCoerce# x
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> ([HsType])
happyOut32 x = unsafeCoerce# x
{-# INLINE happyOut32 #-}
happyIn33 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn33 x = unsafeCoerce# x
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut33 x = unsafeCoerce# x
{-# INLINE happyOut33 #-}
happyIn34 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn34 x = unsafeCoerce# x
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut34 x = unsafeCoerce# x
{-# INLINE happyOut34 #-}
happyIn35 :: (HsDecl) -> (HappyAbsSyn )
happyIn35 x = unsafeCoerce# x
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> (HsDecl)
happyOut35 x = unsafeCoerce# x
{-# INLINE happyOut35 #-}
happyIn36 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn36 x = unsafeCoerce# x
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut36 x = unsafeCoerce# x
{-# INLINE happyOut36 #-}
happyIn37 :: (HsDecl) -> (HappyAbsSyn )
happyIn37 x = unsafeCoerce# x
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> (HsDecl)
happyOut37 x = unsafeCoerce# x
{-# INLINE happyOut37 #-}
happyIn38 :: ([HsName]) -> (HappyAbsSyn )
happyIn38 x = unsafeCoerce# x
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> ([HsName])
happyOut38 x = unsafeCoerce# x
{-# INLINE happyOut38 #-}
happyIn39 :: (HsDecl) -> (HappyAbsSyn )
happyIn39 x = unsafeCoerce# x
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> (HsDecl)
happyOut39 x = unsafeCoerce# x
{-# INLINE happyOut39 #-}
happyIn40 :: (HsSafety) -> (HappyAbsSyn )
happyIn40 x = unsafeCoerce# x
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> (HsSafety)
happyOut40 x = unsafeCoerce# x
{-# INLINE happyOut40 #-}
happyIn41 :: (String) -> (HappyAbsSyn )
happyIn41 x = unsafeCoerce# x
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> (String)
happyOut41 x = unsafeCoerce# x
{-# INLINE happyOut41 #-}
happyIn42 :: (HsName) -> (HappyAbsSyn )
happyIn42 x = unsafeCoerce# x
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> (HsName)
happyOut42 x = unsafeCoerce# x
{-# INLINE happyOut42 #-}
happyIn43 :: (HsType) -> (HappyAbsSyn )
happyIn43 x = unsafeCoerce# x
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> (HsType)
happyOut43 x = unsafeCoerce# x
{-# INLINE happyOut43 #-}
happyIn44 :: (HsType) -> (HappyAbsSyn )
happyIn44 x = unsafeCoerce# x
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> (HsType)
happyOut44 x = unsafeCoerce# x
{-# INLINE happyOut44 #-}
happyIn45 :: (HsType) -> (HappyAbsSyn )
happyIn45 x = unsafeCoerce# x
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> (HsType)
happyOut45 x = unsafeCoerce# x
{-# INLINE happyOut45 #-}
happyIn46 :: (HsQName) -> (HappyAbsSyn )
happyIn46 x = unsafeCoerce# x
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> (HsQName)
happyOut46 x = unsafeCoerce# x
{-# INLINE happyOut46 #-}
happyIn47 :: (HsQualType) -> (HappyAbsSyn )
happyIn47 x = unsafeCoerce# x
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> (HsQualType)
happyOut47 x = unsafeCoerce# x
{-# INLINE happyOut47 #-}
happyIn48 :: (HsContext) -> (HappyAbsSyn )
happyIn48 x = unsafeCoerce# x
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> (HsContext)
happyOut48 x = unsafeCoerce# x
{-# INLINE happyOut48 #-}
happyIn49 :: ([HsType]) -> (HappyAbsSyn )
happyIn49 x = unsafeCoerce# x
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> ([HsType])
happyOut49 x = unsafeCoerce# x
{-# INLINE happyOut49 #-}
happyIn50 :: ((HsName, [HsName])) -> (HappyAbsSyn )
happyIn50 x = unsafeCoerce# x
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> ((HsName, [HsName]))
happyOut50 x = unsafeCoerce# x
{-# INLINE happyOut50 #-}
happyIn51 :: ([HsName]) -> (HappyAbsSyn )
happyIn51 x = unsafeCoerce# x
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> ([HsName])
happyOut51 x = unsafeCoerce# x
{-# INLINE happyOut51 #-}
happyIn52 :: ([HsConDecl]) -> (HappyAbsSyn )
happyIn52 x = unsafeCoerce# x
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> ([HsConDecl])
happyOut52 x = unsafeCoerce# x
{-# INLINE happyOut52 #-}
happyIn53 :: (HsConDecl) -> (HappyAbsSyn )
happyIn53 x = unsafeCoerce# x
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> (HsConDecl)
happyOut53 x = unsafeCoerce# x
{-# INLINE happyOut53 #-}
happyIn54 :: ((HsName, [HsBangType])) -> (HappyAbsSyn )
happyIn54 x = unsafeCoerce# x
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> ((HsName, [HsBangType]))
happyOut54 x = unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyIn55 :: ((HsName, [HsBangType])) -> (HappyAbsSyn )
happyIn55 x = unsafeCoerce# x
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> ((HsName, [HsBangType]))
happyOut55 x = unsafeCoerce# x
{-# INLINE happyOut55 #-}
happyIn56 :: (HsBangType) -> (HappyAbsSyn )
happyIn56 x = unsafeCoerce# x
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> (HsBangType)
happyOut56 x = unsafeCoerce# x
{-# INLINE happyOut56 #-}
happyIn57 :: (HsBangType) -> (HappyAbsSyn )
happyIn57 x = unsafeCoerce# x
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> (HsBangType)
happyOut57 x = unsafeCoerce# x
{-# INLINE happyOut57 #-}
happyIn58 :: ([([HsName],HsBangType)]) -> (HappyAbsSyn )
happyIn58 x = unsafeCoerce# x
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> ([([HsName],HsBangType)])
happyOut58 x = unsafeCoerce# x
{-# INLINE happyOut58 #-}
happyIn59 :: (([HsName],HsBangType)) -> (HappyAbsSyn )
happyIn59 x = unsafeCoerce# x
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> (([HsName],HsBangType))
happyOut59 x = unsafeCoerce# x
{-# INLINE happyOut59 #-}
happyIn60 :: (HsBangType) -> (HappyAbsSyn )
happyIn60 x = unsafeCoerce# x
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> (HsBangType)
happyOut60 x = unsafeCoerce# x
{-# INLINE happyOut60 #-}
happyIn61 :: ([HsQName]) -> (HappyAbsSyn )
happyIn61 x = unsafeCoerce# x
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> ([HsQName])
happyOut61 x = unsafeCoerce# x
{-# INLINE happyOut61 #-}
happyIn62 :: ([HsQName]) -> (HappyAbsSyn )
happyIn62 x = unsafeCoerce# x
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> ([HsQName])
happyOut62 x = unsafeCoerce# x
{-# INLINE happyOut62 #-}
happyIn63 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn63 x = unsafeCoerce# x
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut63 x = unsafeCoerce# x
{-# INLINE happyOut63 #-}
happyIn64 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn64 x = unsafeCoerce# x
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut64 x = unsafeCoerce# x
{-# INLINE happyOut64 #-}
happyIn65 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn65 x = unsafeCoerce# x
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut65 x = unsafeCoerce# x
{-# INLINE happyOut65 #-}
happyIn66 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn66 x = unsafeCoerce# x
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut66 x = unsafeCoerce# x
{-# INLINE happyOut66 #-}
happyIn67 :: (HsDecl) -> (HappyAbsSyn )
happyIn67 x = unsafeCoerce# x
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> (HsDecl)
happyOut67 x = unsafeCoerce# x
{-# INLINE happyOut67 #-}
happyIn68 :: ([HsDecl]) -> (HappyAbsSyn )
happyIn68 x = unsafeCoerce# x
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> ([HsDecl])
happyOut68 x = unsafeCoerce# x
{-# INLINE happyOut68 #-}
happyIn69 :: (HsRhs) -> (HappyAbsSyn )
happyIn69 x = unsafeCoerce# x
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> (HsRhs)
happyOut69 x = unsafeCoerce# x
{-# INLINE happyOut69 #-}
happyIn70 :: ([HsGuardedRhs]) -> (HappyAbsSyn )
happyIn70 x = unsafeCoerce# x
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> ([HsGuardedRhs])
happyOut70 x = unsafeCoerce# x
{-# INLINE happyOut70 #-}
happyIn71 :: (HsGuardedRhs) -> (HappyAbsSyn )
happyIn71 x = unsafeCoerce# x
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> (HsGuardedRhs)
happyOut71 x = unsafeCoerce# x
{-# INLINE happyOut71 #-}
happyIn72 :: (HsExp) -> (HappyAbsSyn )
happyIn72 x = unsafeCoerce# x
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> (HsExp)
happyOut72 x = unsafeCoerce# x
{-# INLINE happyOut72 #-}
happyIn73 :: (HsExp) -> (HappyAbsSyn )
happyIn73 x = unsafeCoerce# x
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> (HsExp)
happyOut73 x = unsafeCoerce# x
{-# INLINE happyOut73 #-}
happyIn74 :: (HsExp) -> (HappyAbsSyn )
happyIn74 x = unsafeCoerce# x
{-# INLINE happyIn74 #-}
happyOut74 :: (HappyAbsSyn ) -> (HsExp)
happyOut74 x = unsafeCoerce# x
{-# INLINE happyOut74 #-}
happyIn75 :: (HsExp) -> (HappyAbsSyn )
happyIn75 x = unsafeCoerce# x
{-# INLINE happyIn75 #-}
happyOut75 :: (HappyAbsSyn ) -> (HsExp)
happyOut75 x = unsafeCoerce# x
{-# INLINE happyOut75 #-}
happyIn76 :: (HsExp) -> (HappyAbsSyn )
happyIn76 x = unsafeCoerce# x
{-# INLINE happyIn76 #-}
happyOut76 :: (HappyAbsSyn ) -> (HsExp)
happyOut76 x = unsafeCoerce# x
{-# INLINE happyOut76 #-}
happyIn77 :: (HsExp) -> (HappyAbsSyn )
happyIn77 x = unsafeCoerce# x
{-# INLINE happyIn77 #-}
happyOut77 :: (HappyAbsSyn ) -> (HsExp)
happyOut77 x = unsafeCoerce# x
{-# INLINE happyOut77 #-}
happyIn78 :: (HsExp) -> (HappyAbsSyn )
happyIn78 x = unsafeCoerce# x
{-# INLINE happyIn78 #-}
happyOut78 :: (HappyAbsSyn ) -> (HsExp)
happyOut78 x = unsafeCoerce# x
{-# INLINE happyOut78 #-}
happyIn79 :: ([HsPat]) -> (HappyAbsSyn )
happyIn79 x = unsafeCoerce# x
{-# INLINE happyIn79 #-}
happyOut79 :: (HappyAbsSyn ) -> ([HsPat])
happyOut79 x = unsafeCoerce# x
{-# INLINE happyOut79 #-}
happyIn80 :: (HsPat) -> (HappyAbsSyn )
happyIn80 x = unsafeCoerce# x
{-# INLINE happyIn80 #-}
happyOut80 :: (HappyAbsSyn ) -> (HsPat)
happyOut80 x = unsafeCoerce# x
{-# INLINE happyOut80 #-}
happyIn81 :: (HsExp) -> (HappyAbsSyn )
happyIn81 x = unsafeCoerce# x
{-# INLINE happyIn81 #-}
happyOut81 :: (HappyAbsSyn ) -> (HsExp)
happyOut81 x = unsafeCoerce# x
{-# INLINE happyOut81 #-}
happyIn82 :: (HsExp) -> (HappyAbsSyn )
happyIn82 x = unsafeCoerce# x
{-# INLINE happyIn82 #-}
happyOut82 :: (HappyAbsSyn ) -> (HsExp)
happyOut82 x = unsafeCoerce# x
{-# INLINE happyOut82 #-}
happyIn83 :: (HsExp) -> (HappyAbsSyn )
happyIn83 x = unsafeCoerce# x
{-# INLINE happyIn83 #-}
happyOut83 :: (HappyAbsSyn ) -> (HsExp)
happyOut83 x = unsafeCoerce# x
{-# INLINE happyOut83 #-}
happyIn84 :: (Int) -> (HappyAbsSyn )
happyIn84 x = unsafeCoerce# x
{-# INLINE happyIn84 #-}
happyOut84 :: (HappyAbsSyn ) -> (Int)
happyOut84 x = unsafeCoerce# x
{-# INLINE happyOut84 #-}
happyIn85 :: ([HsExp]) -> (HappyAbsSyn )
happyIn85 x = unsafeCoerce# x
{-# INLINE happyIn85 #-}
happyOut85 :: (HappyAbsSyn ) -> ([HsExp])
happyOut85 x = unsafeCoerce# x
{-# INLINE happyOut85 #-}
happyIn86 :: (HsExp) -> (HappyAbsSyn )
happyIn86 x = unsafeCoerce# x
{-# INLINE happyIn86 #-}
happyOut86 :: (HappyAbsSyn ) -> (HsExp)
happyOut86 x = unsafeCoerce# x
{-# INLINE happyOut86 #-}
happyIn87 :: ([HsExp]) -> (HappyAbsSyn )
happyIn87 x = unsafeCoerce# x
{-# INLINE happyIn87 #-}
happyOut87 :: (HappyAbsSyn ) -> ([HsExp])
happyOut87 x = unsafeCoerce# x
{-# INLINE happyOut87 #-}
happyIn88 :: ([HsStmt]) -> (HappyAbsSyn )
happyIn88 x = unsafeCoerce# x
{-# INLINE happyIn88 #-}
happyOut88 :: (HappyAbsSyn ) -> ([HsStmt])
happyOut88 x = unsafeCoerce# x
{-# INLINE happyOut88 #-}
happyIn89 :: (HsStmt) -> (HappyAbsSyn )
happyIn89 x = unsafeCoerce# x
{-# INLINE happyIn89 #-}
happyOut89 :: (HappyAbsSyn ) -> (HsStmt)
happyOut89 x = unsafeCoerce# x
{-# INLINE happyOut89 #-}
happyIn90 :: ([HsAlt]) -> (HappyAbsSyn )
happyIn90 x = unsafeCoerce# x
{-# INLINE happyIn90 #-}
happyOut90 :: (HappyAbsSyn ) -> ([HsAlt])
happyOut90 x = unsafeCoerce# x
{-# INLINE happyOut90 #-}
happyIn91 :: ([HsAlt]) -> (HappyAbsSyn )
happyIn91 x = unsafeCoerce# x
{-# INLINE happyIn91 #-}
happyOut91 :: (HappyAbsSyn ) -> ([HsAlt])
happyOut91 x = unsafeCoerce# x
{-# INLINE happyOut91 #-}
happyIn92 :: ([HsAlt]) -> (HappyAbsSyn )
happyIn92 x = unsafeCoerce# x
{-# INLINE happyIn92 #-}
happyOut92 :: (HappyAbsSyn ) -> ([HsAlt])
happyOut92 x = unsafeCoerce# x
{-# INLINE happyOut92 #-}
happyIn93 :: (HsAlt) -> (HappyAbsSyn )
happyIn93 x = unsafeCoerce# x
{-# INLINE happyIn93 #-}
happyOut93 :: (HappyAbsSyn ) -> (HsAlt)
happyOut93 x = unsafeCoerce# x
{-# INLINE happyOut93 #-}
happyIn94 :: (HsGuardedAlts) -> (HappyAbsSyn )
happyIn94 x = unsafeCoerce# x
{-# INLINE happyIn94 #-}
happyOut94 :: (HappyAbsSyn ) -> (HsGuardedAlts)
happyOut94 x = unsafeCoerce# x
{-# INLINE happyOut94 #-}
happyIn95 :: ([HsGuardedAlt]) -> (HappyAbsSyn )
happyIn95 x = unsafeCoerce# x
{-# INLINE happyIn95 #-}
happyOut95 :: (HappyAbsSyn ) -> ([HsGuardedAlt])
happyOut95 x = unsafeCoerce# x
{-# INLINE happyOut95 #-}
happyIn96 :: (HsGuardedAlt) -> (HappyAbsSyn )
happyIn96 x = unsafeCoerce# x
{-# INLINE happyIn96 #-}
happyOut96 :: (HappyAbsSyn ) -> (HsGuardedAlt)
happyOut96 x = unsafeCoerce# x
{-# INLINE happyOut96 #-}
happyIn97 :: (HsPat) -> (HappyAbsSyn )
happyIn97 x = unsafeCoerce# x
{-# INLINE happyIn97 #-}
happyOut97 :: (HappyAbsSyn ) -> (HsPat)
happyOut97 x = unsafeCoerce# x
{-# INLINE happyOut97 #-}
happyIn98 :: ([HsStmt]) -> (HappyAbsSyn )
happyIn98 x = unsafeCoerce# x
{-# INLINE happyIn98 #-}
happyOut98 :: (HappyAbsSyn ) -> ([HsStmt])
happyOut98 x = unsafeCoerce# x
{-# INLINE happyOut98 #-}
happyIn99 :: ([HsStmt]) -> (HappyAbsSyn )
happyIn99 x = unsafeCoerce# x
{-# INLINE happyIn99 #-}
happyOut99 :: (HappyAbsSyn ) -> ([HsStmt])
happyOut99 x = unsafeCoerce# x
{-# INLINE happyOut99 #-}
happyIn100 :: ([HsFieldUpdate]) -> (HappyAbsSyn )
happyIn100 x = unsafeCoerce# x
{-# INLINE happyIn100 #-}
happyOut100 :: (HappyAbsSyn ) -> ([HsFieldUpdate])
happyOut100 x = unsafeCoerce# x
{-# INLINE happyOut100 #-}
happyIn101 :: (HsFieldUpdate) -> (HappyAbsSyn )
happyIn101 x = unsafeCoerce# x
{-# INLINE happyIn101 #-}
happyOut101 :: (HappyAbsSyn ) -> (HsFieldUpdate)
happyOut101 x = unsafeCoerce# x
{-# INLINE happyOut101 #-}
happyIn102 :: (HsExp) -> (HappyAbsSyn )
happyIn102 x = unsafeCoerce# x
{-# INLINE happyIn102 #-}
happyOut102 :: (HappyAbsSyn ) -> (HsExp)
happyOut102 x = unsafeCoerce# x
{-# INLINE happyOut102 #-}
happyIn103 :: (HsName) -> (HappyAbsSyn )
happyIn103 x = unsafeCoerce# x
{-# INLINE happyIn103 #-}
happyOut103 :: (HappyAbsSyn ) -> (HsName)
happyOut103 x = unsafeCoerce# x
{-# INLINE happyOut103 #-}
happyIn104 :: (HsQName) -> (HappyAbsSyn )
happyIn104 x = unsafeCoerce# x
{-# INLINE happyIn104 #-}
happyOut104 :: (HappyAbsSyn ) -> (HsQName)
happyOut104 x = unsafeCoerce# x
{-# INLINE happyOut104 #-}
happyIn105 :: (HsName) -> (HappyAbsSyn )
happyIn105 x = unsafeCoerce# x
{-# INLINE happyIn105 #-}
happyOut105 :: (HappyAbsSyn ) -> (HsName)
happyOut105 x = unsafeCoerce# x
{-# INLINE happyOut105 #-}
happyIn106 :: (HsQName) -> (HappyAbsSyn )
happyIn106 x = unsafeCoerce# x
{-# INLINE happyIn106 #-}
happyOut106 :: (HappyAbsSyn ) -> (HsQName)
happyOut106 x = unsafeCoerce# x
{-# INLINE happyOut106 #-}
happyIn107 :: (HsName) -> (HappyAbsSyn )
happyIn107 x = unsafeCoerce# x
{-# INLINE happyIn107 #-}
happyOut107 :: (HappyAbsSyn ) -> (HsName)
happyOut107 x = unsafeCoerce# x
{-# INLINE happyOut107 #-}
happyIn108 :: (HsQName) -> (HappyAbsSyn )
happyIn108 x = unsafeCoerce# x
{-# INLINE happyIn108 #-}
happyOut108 :: (HappyAbsSyn ) -> (HsQName)
happyOut108 x = unsafeCoerce# x
{-# INLINE happyOut108 #-}
happyIn109 :: (HsQName) -> (HappyAbsSyn )
happyIn109 x = unsafeCoerce# x
{-# INLINE happyIn109 #-}
happyOut109 :: (HappyAbsSyn ) -> (HsQName)
happyOut109 x = unsafeCoerce# x
{-# INLINE happyOut109 #-}
happyIn110 :: (HsName) -> (HappyAbsSyn )
happyIn110 x = unsafeCoerce# x
{-# INLINE happyIn110 #-}
happyOut110 :: (HappyAbsSyn ) -> (HsName)
happyOut110 x = unsafeCoerce# x
{-# INLINE happyOut110 #-}
happyIn111 :: (HsQName) -> (HappyAbsSyn )
happyIn111 x = unsafeCoerce# x
{-# INLINE happyIn111 #-}
happyOut111 :: (HappyAbsSyn ) -> (HsQName)
happyOut111 x = unsafeCoerce# x
{-# INLINE happyOut111 #-}
happyIn112 :: (HsOp) -> (HappyAbsSyn )
happyIn112 x = unsafeCoerce# x
{-# INLINE happyIn112 #-}
happyOut112 :: (HappyAbsSyn ) -> (HsOp)
happyOut112 x = unsafeCoerce# x
{-# INLINE happyOut112 #-}
happyIn113 :: (HsQOp) -> (HappyAbsSyn )
happyIn113 x = unsafeCoerce# x
{-# INLINE happyIn113 #-}
happyOut113 :: (HappyAbsSyn ) -> (HsQOp)
happyOut113 x = unsafeCoerce# x
{-# INLINE happyOut113 #-}
happyIn114 :: (HsQOp) -> (HappyAbsSyn )
happyIn114 x = unsafeCoerce# x
{-# INLINE happyIn114 #-}
happyOut114 :: (HappyAbsSyn ) -> (HsQOp)
happyOut114 x = unsafeCoerce# x
{-# INLINE happyOut114 #-}
happyIn115 :: (HsQName) -> (HappyAbsSyn )
happyIn115 x = unsafeCoerce# x
{-# INLINE happyIn115 #-}
happyOut115 :: (HappyAbsSyn ) -> (HsQName)
happyOut115 x = unsafeCoerce# x
{-# INLINE happyOut115 #-}
happyIn116 :: (HsQName) -> (HappyAbsSyn )
happyIn116 x = unsafeCoerce# x
{-# INLINE happyIn116 #-}
happyOut116 :: (HappyAbsSyn ) -> (HsQName)
happyOut116 x = unsafeCoerce# x
{-# INLINE happyOut116 #-}
happyIn117 :: (HsName) -> (HappyAbsSyn )
happyIn117 x = unsafeCoerce# x
{-# INLINE happyIn117 #-}
happyOut117 :: (HappyAbsSyn ) -> (HsName)
happyOut117 x = unsafeCoerce# x
{-# INLINE happyOut117 #-}
happyIn118 :: (HsQName) -> (HappyAbsSyn )
happyIn118 x = unsafeCoerce# x
{-# INLINE happyIn118 #-}
happyOut118 :: (HappyAbsSyn ) -> (HsQName)
happyOut118 x = unsafeCoerce# x
{-# INLINE happyOut118 #-}
happyIn119 :: (HsName) -> (HappyAbsSyn )
happyIn119 x = unsafeCoerce# x
{-# INLINE happyIn119 #-}
happyOut119 :: (HappyAbsSyn ) -> (HsName)
happyOut119 x = unsafeCoerce# x
{-# INLINE happyOut119 #-}
happyIn120 :: (HsQName) -> (HappyAbsSyn )
happyIn120 x = unsafeCoerce# x
{-# INLINE happyIn120 #-}
happyOut120 :: (HappyAbsSyn ) -> (HsQName)
happyOut120 x = unsafeCoerce# x
{-# INLINE happyOut120 #-}
happyIn121 :: (HsName) -> (HappyAbsSyn )
happyIn121 x = unsafeCoerce# x
{-# INLINE happyIn121 #-}
happyOut121 :: (HappyAbsSyn ) -> (HsName)
happyOut121 x = unsafeCoerce# x
{-# INLINE happyOut121 #-}
happyIn122 :: (HsQName) -> (HappyAbsSyn )
happyIn122 x = unsafeCoerce# x
{-# INLINE happyIn122 #-}
happyOut122 :: (HappyAbsSyn ) -> (HsQName)
happyOut122 x = unsafeCoerce# x
{-# INLINE happyOut122 #-}
happyIn123 :: (HsQName) -> (HappyAbsSyn )
happyIn123 x = unsafeCoerce# x
{-# INLINE happyIn123 #-}
happyOut123 :: (HappyAbsSyn ) -> (HsQName)
happyOut123 x = unsafeCoerce# x
{-# INLINE happyOut123 #-}
happyIn124 :: (HsName) -> (HappyAbsSyn )
happyIn124 x = unsafeCoerce# x
{-# INLINE happyIn124 #-}
happyOut124 :: (HappyAbsSyn ) -> (HsName)
happyOut124 x = unsafeCoerce# x
{-# INLINE happyOut124 #-}
happyIn125 :: (HsName) -> (HappyAbsSyn )
happyIn125 x = unsafeCoerce# x
{-# INLINE happyIn125 #-}
happyOut125 :: (HappyAbsSyn ) -> (HsName)
happyOut125 x = unsafeCoerce# x
{-# INLINE happyOut125 #-}
happyIn126 :: (HsQName) -> (HappyAbsSyn )
happyIn126 x = unsafeCoerce# x
{-# INLINE happyIn126 #-}
happyOut126 :: (HappyAbsSyn ) -> (HsQName)
happyOut126 x = unsafeCoerce# x
{-# INLINE happyOut126 #-}
happyIn127 :: (HsLiteral) -> (HappyAbsSyn )
happyIn127 x = unsafeCoerce# x
{-# INLINE happyIn127 #-}
happyOut127 :: (HappyAbsSyn ) -> (HsLiteral)
happyOut127 x = unsafeCoerce# x
{-# INLINE happyOut127 #-}
happyIn128 :: (SrcLoc) -> (HappyAbsSyn )
happyIn128 x = unsafeCoerce# x
{-# INLINE happyIn128 #-}
happyOut128 :: (HappyAbsSyn ) -> (SrcLoc)
happyOut128 x = unsafeCoerce# x
{-# INLINE happyOut128 #-}
happyIn129 :: (()) -> (HappyAbsSyn )
happyIn129 x = unsafeCoerce# x
{-# INLINE happyIn129 #-}
happyOut129 :: (HappyAbsSyn ) -> (())
happyOut129 x = unsafeCoerce# x
{-# INLINE happyOut129 #-}
happyIn130 :: (()) -> (HappyAbsSyn )
happyIn130 x = unsafeCoerce# x
{-# INLINE happyIn130 #-}
happyOut130 :: (HappyAbsSyn ) -> (())
happyOut130 x = unsafeCoerce# x
{-# INLINE happyOut130 #-}
happyIn131 :: (Module) -> (HappyAbsSyn )
happyIn131 x = unsafeCoerce# x
{-# INLINE happyIn131 #-}
happyOut131 :: (HappyAbsSyn ) -> (Module)
happyOut131 x = unsafeCoerce# x
{-# INLINE happyOut131 #-}
happyIn132 :: (HsName) -> (HappyAbsSyn )
happyIn132 x = unsafeCoerce# x
{-# INLINE happyIn132 #-}
happyOut132 :: (HappyAbsSyn ) -> (HsName)
happyOut132 x = unsafeCoerce# x
{-# INLINE happyOut132 #-}
happyIn133 :: (HsName) -> (HappyAbsSyn )
happyIn133 x = unsafeCoerce# x
{-# INLINE happyIn133 #-}
happyOut133 :: (HappyAbsSyn ) -> (HsName)
happyOut133 x = unsafeCoerce# x
{-# INLINE happyOut133 #-}
happyIn134 :: (HsQName) -> (HappyAbsSyn )
happyIn134 x = unsafeCoerce# x
{-# INLINE happyIn134 #-}
happyOut134 :: (HappyAbsSyn ) -> (HsQName)
happyOut134 x = unsafeCoerce# x
{-# INLINE happyOut134 #-}
happyIn135 :: (HsQName) -> (HappyAbsSyn )
happyIn135 x = unsafeCoerce# x
{-# INLINE happyIn135 #-}
happyOut135 :: (HappyAbsSyn ) -> (HsQName)
happyOut135 x = unsafeCoerce# x
{-# INLINE happyOut135 #-}
happyIn136 :: (HsName) -> (HappyAbsSyn )
happyIn136 x = unsafeCoerce# x
{-# INLINE happyIn136 #-}
happyOut136 :: (HappyAbsSyn ) -> (HsName)
happyOut136 x = unsafeCoerce# x
{-# INLINE happyOut136 #-}
happyInTok :: Token -> (HappyAbsSyn )
happyInTok x = unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> Token
happyOutTok x = unsafeCoerce# x
{-# INLINE happyOutTok #-}

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\x00\x00\x49\x04\x3c\x04\xfe\xff\x00\x00\x00\x00\x00\x00\x21\x03\x6f\x04\x00\x00\x00\x00\x64\x04\x00\x00\x6d\x02\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfc\x09\x00\x00\x00\x00\x41\x04\x00\x00\xff\x00\x66\x04\x5e\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x03\x65\x04\x00\x00\x00\x00\x21\x03\x57\x04\x5d\x04\xa7\x02\xa4\x02\x00\x00\x6d\x04\x00\x00\x55\x04\x00\x00\x00\x00\xe2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\x0a\xd5\x0b\x00\x00\xd9\x04\xd9\x04\x8e\x0c\x53\x02\x53\x02\x56\x04\x54\x04\x0d\x01\x24\x04\x00\x00\x00\x00\x00\x00\x53\x02\x53\x02\x00\x00\xed\x02\x52\x04\xad\x02\x52\x04\x00\x00\x00\x00\x00\x00\x2d\x0a\x45\x04\x00\x00\x5c\x04\x00\x00\x00\x00\x85\x04\x00\x00\x00\x00\x3b\x04\x36\x04\x00\x00\x00\x00\x60\x03\x00\x00\x46\x03\x5f\x02\x1b\x04\x21\x03\x00\x00\x53\x04\x4c\x04\x00\x00\xb0\x0b\xb0\x0b\x94\x02\x34\x04\x12\x04\x14\x04\x00\x00\x00\x00\x7f\x03\x00\x00\x2a\x04\x00\x00\x8e\x0c\x39\x04\x6d\x04\x00\x00\x3c\x02\x33\x04\x31\x04\x00\x00\x3e\x02\x01\x03\x39\x02\x2b\x02\x00\x00\x00\x00\x8e\x0c\x27\x04\x00\x00\x00\x00\x26\x04\x00\x00\x00\x00\x00\x00\x25\x04\x1e\x04\x00\x00\x00\x00\x00\x00\x00\x00\xd9\x04\xcb\x02\x00\x00\x0a\x04\x15\x04\x00\x00\x00\x00\x00\x00\xc6\x0c\x00\x00\x00\x00\x00\x00\xe4\x01\x00\x00\x8e\x0c\x1a\x04\x53\x02\x07\x04\x00\x00\x00\x00\x00\x00\x18\x02\x0b\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x04\xc6\x00\x00\x00\x00\x00\x00\x00\x02\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x03\x00\x00\x00\x00\x0e\x04\x00\x00\x00\x00\x00\x00\x00\x00\x06\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\xa5\x03\x00\x00\x00\x00\x03\x04\x01\x04\x00\x00\x00\x00\x8e\x0c\x00\x00\x00\x00\xf9\x03\x00\x00\x05\x04\xc5\x02\x00\x00\xfe\x03\x49\x02\x00\x00\x00\x00\xfd\x03\xfc\x03\xfb\x03\x00\x00\x00\x00\xfa\x03\x41\x03\x00\x00\x8e\x0c\x00\x00\x00\x00\x8a\x0b\x31\x01\x00\x00\x00\x00\x8e\x0c\x8e\x0c\x00\x00\x8e\x0c\x65\x0b\x69\x0c\xd8\x03\x00\x00\x00\x00\xcd\x03\xd9\x04\x44\x0c\xf1\x03\x00\x00\xee\x03\x00\x00\xef\x03\xe6\x03\x93\x04\xe0\x03\xe4\x03\xf0\x0a\x00\x00\xe3\x03\x40\x0b\xe2\x03\x18\x00\xda\x03\xe2\x02\xac\x03\x00\x00\xcb\x03\xc2\x03\x00\x00\xbc\x01\xb0\x01\x0c\x01\x00\x00\xc5\x03\x53\x02\x00\x00\x00\x00\x53\x02\xb4\x03\x53\x02\x00\x00\x00\x00\x00\x00\xab\x03\x50\x02\x00\x00\x00\x00\x00\x00\x00\x00\x53\x02\x00\x00\x53\x02\x00\x00\x00\x00\x00\x00\x0d\x00\xfe\x02\xb6\x03\x00\x00\x00\x00\xb1\x03\x00\x00\x00\x00\xda\x00\x00\x00\x00\x00\x9f\x03\x6d\x03\x00\x00\xd5\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x04\x00\x00\x00\x00\x44\x0c\x2d\x03\xaa\x03\x18\x00\x44\x0c\x00\x00\xa0\x03\x00\x00\x00\x00\xa4\x03\x00\x00\x9a\x03\x00\x00\x00\x00\x53\x02\x00\x00\x00\x00\x00\x00\x00\x00\x44\x0c\x00\x00\xa6\x03\x00\x00\x8a\x03\x00\x00\x00\x00\x99\x03\x1f\x00\x8d\x03\x7e\x03\x7d\x03\x00\x00\x85\x03\x00\x00\x84\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x0c\x00\x00\x00\x00\x00\x00\x1a\x0b\x5c\x03\x6f\x03\x1f\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x0c\x57\x03\x00\x00\xfa\x0b\x72\x03\x6c\x03\x18\x00\x00\x00\x00\x00\x6e\x03\x00\x00\xfa\x0b\xf5\x0a\x5b\x03\x73\x03\x00\x00\x66\x03\x00\x00\x00\x00\x00\x00\x65\x03\x00\x00\xed\x03\x45\x03\x18\x00\x00\x00\x00\x00\x7e\x04\x00\x00\x8c\x04\x50\x03\x56\x03\x52\x03\x46\x03\x53\x02\x00\x00\x00\x00\xc9\x02\x71\x02\x22\x02\x4f\x03\x00\x00\x00\x00\x00\x00\x53\x02\x53\x02\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x0c\x70\x01\x30\x03\x3f\x03\x00\x00\x53\x02\x00\x00\x42\x03\x00\x00\xe1\x02\x00\x00\x3e\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x0c\x00\x00\xfa\x0b\xad\x03\x3c\x03\x00\x00\xfa\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x02\x23\x03\x2d\x04\x2c\x03\xf3\x00\x00\x00\x00\x00\xf5\x0a\x00\x00\x00\x00\x53\x02\x33\x03\x20\x03\x00\x00\x00\x00\x00\x00\x1a\x03\x2d\x04\x0d\x03\x00\x00\x00\x00\x89\x02\x00\x00\x9f\x00\x7a\x02\x00\x00\x00\x00\x00\x00\x00\x00\x04\x03\x21\x02\x00\x00\x95\x01\xf1\x02\xc7\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x97\x02\xca\x02\x00\x00\xae\x02\xfa\x0b\x00\x00\xfa\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\x00\x00\x00\xb4\x02\x00\x00\x00\x00\x53\x02\x00\x00\x00\x00\x00\x00\x00\x00\x7b\x02\xfa\x0b\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x04\x00\x1c\x02\x00\x00\x00\x00\x02\x00\x00\x00\x3b\x03\xf9\x02\x12\x02\x67\x01\x00\x00\x00\x00\x00\x00\x00\x00\x06\x00\xf7\x01\x00\x00\x00\x00\x00\x00\xde\x02\x00\x00\x00\x00\x00\x00\xa1\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\x00\x00\xff\xff\x00\x00\x61\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x02\x00\x00\xe3\x01\x01\x00\xff\x01\x00\x00\x97\x03\x00\x00\x5b\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x04\x49\x06\x00\x00\x56\x0a\x7f\x09\xad\x08\x1c\x01\x04\x01\x00\x00\x4e\x00\x00\x00\xfa\x01\x00\x00\x00\x00\x00\x00\xe7\x00\xdb\x00\x3c\x00\x36\x00\x00\x00\x17\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\x00\x00\x00\x00\x00\x00\x00\xd5\x01\x00\x00\xe9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\x00\x8d\x01\xb4\x01\x64\x01\x00\x00\x00\x00\x00\x00\x00\x00\x19\x06\xe9\x05\x37\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x0f\x0b\x00\x00\x00\x00\x35\x01\x9b\x08\x0b\x00\x3b\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xea\x0a\x00\x00\x00\x00\x00\x00\x00\x00\x1f\x09\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x82\x02\x00\x00\x20\x0a\x1b\x01\x00\x00\x6f\x01\x15\x00\x00\x00\x00\x00\x00\x00\x4f\x09\x00\x00\x00\x00\x00\x00\x00\x00\x42\x02\x6b\x08\x73\x00\xb6\x00\x60\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x13\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\xd0\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x08\x00\x00\x00\x00\x00\x00\x00\x00\xef\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x0a\x00\x00\x59\x08\x00\x00\x00\x00\x61\x09\x00\x00\x25\x01\x00\x00\x29\x08\x17\x08\x00\x00\xe7\x07\xd5\x07\xc9\x04\x00\x00\x22\x01\x20\x01\x00\x00\x9b\x09\x61\x09\xc7\xff\x00\x00\xea\xff\x4c\x00\x00\x00\x00\x00\xe9\x01\x00\x00\x00\x00\xc4\x0a\x1a\x01\x00\x00\xb9\x05\xe9\xff\x0b\x01\x61\x01\x55\x01\x57\x01\x00\x00\xe4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x81\x01\x34\x00\x00\x00\x75\x01\x3d\x01\x69\x01\x00\x00\x00\x00\x00\x00\x1d\x01\xaf\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x00\x00\x4a\x01\x00\x00\xbd\x00\x31\x00\x24\x03\xcb\x00\x19\x01\x00\x00\x00\x00\xed\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x05\x00\x00\xd1\x00\x00\x00\x00\x00\x00\x00\x43\x00\x3d\x00\x00\x00\x1b\x0a\x00\x00\x00\x00\xa5\x07\xf8\xff\x00\x00\x65\x00\x93\x07\x00\x00\x00\x00\x00\x00\x39\x00\xe5\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x63\x07\x00\x00\xee\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x03\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x51\x07\x00\x00\x00\x00\x00\x00\x21\x07\x00\x00\x00\x00\x59\x05\x00\x00\x00\x00\x00\x00\x2d\x02\x00\x00\x75\x00\x00\x00\x00\x00\x0f\x07\xf7\xff\x00\x00\x1a\x00\x00\x00\x29\x00\x59\x03\x00\x00\xdf\x06\x29\x05\x00\x00\x00\x00\xc1\xff\xec\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\x00\x00\xde\xff\x00\x00\x00\x00\xe9\x01\x00\x00\xa9\x01\x8e\x01\x00\x00\x00\x00\x79\x00\xe5\x01\x00\x00\x00\x00\x00\x00\x8f\x00\x9a\x01\x00\x00\x3b\x00\x00\x00\x00\x00\xd1\x01\xcc\x01\x00\x00\x00\x00\xc4\x01\x00\x00\x47\x05\xfa\xff\x00\x00\x00\x00\x00\x00\x34\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4d\x01\x00\x00\x00\x00\x00\x00\x12\x01\x00\x00\x31\x09\x00\x00\xcd\x06\x0e\x00\x00\x00\x00\x00\x9d\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb4\x04\x0e\x01\xcd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf9\x04\x00\x00\x00\x00\xf4\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdf\xff\x00\x00\x00\x00\x00\x00\xc5\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfe\x00\x28\x01\x00\x00\x28\x00\x00\x00\x16\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x03\x00\x00\xee\xff\x5e\x00\x00\x00\x00\x00\x8b\x06\x00\x00\xdd\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x06\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdb\xfe\x00\x00\x00\x00\x00\x00\xda\xfe\xfd\xff\xf4\xff\xf4\xff\x00\x00\xf2\xff\xd7\xfe\xd6\xfe\x00\x00\xf5\xff\xf7\xff\x00\x00\xfb\xff\xd8\xfe\xd9\xfe\xf4\xff\xe4\xff\xb2\xff\xf9\xff\xf4\xff\xc3\xff\xbb\xff\xb3\xff\xbc\xff\xb1\xff\xdb\xfe\xf6\xff\xfc\xff\x00\x00\xf3\xff\xdb\xfe\x00\x00\xee\xff\xec\xff\xeb\xff\x10\xff\xf8\xfe\xd3\xfe\xef\xfe\x00\x00\xea\xff\xdb\xfe\xef\xff\x00\x00\xda\xfe\xcc\xff\x00\x00\xdb\xfe\x5b\xff\x54\xff\x52\xff\x4c\xff\x49\xff\x47\xff\x48\xff\x13\xff\x0c\xff\x46\xff\xdf\xfe\xdd\xfe\xde\xfe\xdc\xfe\xdb\xfe\xdb\xfe\x40\xff\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\xda\xfe\x00\x00\xe1\xff\xca\xff\xc9\xff\xc8\xff\xdb\xfe\xdb\xfe\xdb\xfe\xf5\xff\xc5\xff\xf5\xff\xf8\xff\xe5\xff\xfa\xff\xc4\xff\xdb\xfe\x00\x00\xd4\xfe\x00\x00\x8d\xff\x93\xff\xa1\xff\x9f\xff\x9e\xff\x00\x00\x00\x00\xd1\xfe\x99\xff\x00\x00\x9d\xff\xdb\xfe\xdb\xfe\x6f\xff\x00\x00\xe2\xff\x00\x00\x00\x00\x55\xff\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\x72\xff\x00\x00\x61\xff\x60\xff\x5f\xff\x5d\xff\x48\xff\xdb\xfe\xdb\xfe\xda\xfe\x56\xff\x4d\xff\x3b\xff\x00\x00\x3a\xff\x15\xff\x00\x00\xdb\xfe\x00\x00\x00\x00\xfc\xfe\xfb\xfe\xdb\xfe\x02\xff\xf9\xfe\xec\xfe\x00\x00\x06\xff\xe9\xfe\xe7\xfe\xe6\xfe\x00\x00\x16\xff\x3e\xff\xdb\xfe\xfa\xfe\xdb\xfe\xdb\xfe\x53\xff\x68\xff\x66\xff\x64\xff\xfe\xfe\xfd\xfe\xdb\xfe\x02\xff\x08\xff\xe8\xfe\x00\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xcb\xff\xfe\xff\xe6\xff\x00\x00\xdb\xfe\xf6\xfe\xf7\xfe\xed\xfe\xee\xfe\xf5\xfe\xf4\xfe\xf3\xfe\xf2\xfe\xf1\xfe\xf0\xfe\x00\x00\xdb\xfe\xf0\xff\xed\xff\xf1\xff\x00\x00\xd0\xff\xcf\xff\xce\xff\x12\xff\x0e\xff\x00\x00\xdb\xfe\xe8\xff\x00\x00\xe5\xfe\xe0\xfe\xe4\xfe\xe3\xfe\xcd\xff\x00\xff\xff\xfe\xc6\xff\x04\xff\x0a\xff\x00\x00\xdb\xfe\xae\xff\xad\xff\x00\x00\xdb\xfe\x67\xff\x00\x00\x00\x00\xea\xfe\xeb\xfe\xdb\xfe\x5c\xff\x65\xff\x00\x00\x6a\xff\xda\xfe\x00\x00\x18\xff\x00\x00\x00\x00\x4b\xff\x4e\xff\x00\x00\xe2\xfe\xe1\xfe\x0f\xff\x0b\xff\x00\x00\xdb\xfe\x44\xff\xdb\xfe\x14\xff\x3f\xff\xdb\xfe\x00\x00\xdb\xfe\x45\xff\xdb\xfe\xdb\xfe\x43\xff\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\xf4\xff\xf4\xff\x00\x00\xdb\xfe\xdb\xfe\xda\xfe\xbf\xff\xda\xfe\xdb\xfe\x00\x00\xb9\xff\xa1\xff\xba\xff\x1a\xff\x5f\xff\xdb\xfe\x00\x00\xdb\xfe\xda\xfe\x00\x00\xa5\xff\xa7\xff\xdf\xff\xbe\xff\xda\xfe\x00\x00\x96\xff\x00\x00\x00\x00\x00\x00\x98\xff\x00\x00\xdb\xfe\xdb\xfe\xa0\xff\xdb\xfe\xdb\xfe\xdb\xfe\xc2\xff\x8e\xff\xa2\xff\x79\xff\xdb\xfe\x94\xff\x97\xff\x95\xff\x9c\xff\xdb\xfe\x9a\xff\xdb\xfe\x9b\xff\xf4\xff\xf4\xff\xdd\xff\x00\x00\xa5\xff\xa9\xff\xa8\xff\xdb\xfe\xa6\xff\x20\xff\x00\x00\x1c\xff\x21\xff\x00\x00\x1b\xff\xbd\xff\x79\xff\x8b\xff\x73\xff\x57\xff\xf4\xff\xf4\xff\x5e\xff\xdb\xfe\x50\xff\x4f\xff\xdb\xfe\xb6\xff\x00\x00\x00\x00\xdb\xfe\x2f\xff\x35\xff\x31\xff\xdb\xfe\xda\xfe\x37\xff\x33\xff\x34\xff\x3c\xff\xdb\xfe\x42\xff\x3d\xff\x41\xff\x05\xff\xdb\xfe\x4a\xff\xdb\xfe\x69\xff\x00\x00\x01\xff\x07\xff\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xfe\xe9\xff\x00\x00\xe7\xff\xdb\xfe\xd1\xff\x0d\xff\xc7\xff\x03\xff\x09\xff\x11\xff\xdb\xfe\x19\xff\x17\xff\x62\xff\xdb\xfe\x2e\xff\x00\x00\xdb\xfe\x59\xff\xaf\xff\xb0\xff\xf4\xff\xb4\xff\xdb\xfe\x00\x00\x51\xff\xdb\xfe\xdb\xfe\x00\x00\x00\x00\xc1\xff\xdb\xfe\xdb\xfe\x1d\xff\xdb\xfe\xdb\xfe\x00\x00\x00\x00\xdb\xfe\xdb\xfe\xe0\xff\xe3\xff\xde\xff\x00\x00\xda\xff\x6d\xff\x00\x00\x00\x00\x90\xff\x91\xff\x86\xff\x8a\xff\x85\xff\xdb\xfe\x00\x00\xef\xfe\xdb\xfe\xdb\xfe\xc0\xff\x7f\xff\x00\x00\xdb\xfe\xdb\xfe\x00\x00\xdb\xfe\x82\xff\x83\xff\xdb\xfe\xdb\xfe\x70\xff\x71\xff\xf4\xff\x6b\xff\xdb\xfe\xdb\xfe\x00\x00\x00\x00\xa4\xff\xdb\xfe\x1f\xff\x00\x00\xd2\xfe\x00\x00\x78\xff\xdb\xfe\x8c\xff\x2c\xff\x2d\xff\xf4\xff\x29\xff\xdb\xfe\x5a\xff\xdb\xfe\xf5\xff\xb7\xff\x32\xff\xdb\xfe\x36\xff\x63\xff\x30\xff\xb5\xff\x58\xff\xdb\xfe\xdb\xfe\xf5\xff\x2b\xff\x00\x00\x74\xff\x77\xff\xdb\xfe\xaa\xff\xa3\xff\xdb\xfe\x00\x00\xee\xff\xd7\xff\xd6\xff\xd5\xfe\xd5\xff\xf5\xff\x6e\xff\x84\xff\x81\xff\x80\xff\x89\xff\x00\x00\x00\x00\x7d\xff\xac\xff\x88\xff\x87\xff\xdb\xfe\xdb\xfe\x6c\xff\xdb\xfe\x00\x00\xdb\xfe\xdb\xff\xab\xff\x1e\xff\x76\xff\xdb\xfe\x2a\xff\x68\xff\x26\xff\x24\xff\x00\x00\xdb\xfe\x27\xff\xdb\xfe\x25\xff\x28\xff\x75\xff\xd8\xff\xdc\xff\x00\x00\xd3\xff\x00\x00\x7b\xff\x7c\xff\xdb\xfe\x7e\xff\x7a\xff\xd4\xff\xd2\xff\x00\x00\xdb\xfe\x23\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x07\x00\x01\x00\x01\x00\x00\x00\x20\x00\x07\x00\x08\x00\x09\x00\x20\x00\x20\x00\x11\x00\x12\x00\x15\x00\x10\x00\x20\x00\x0a\x00\x0b\x00\x26\x00\x26\x00\x3e\x00\x3f\x00\x1e\x00\x1f\x00\x00\x00\x21\x00\x0d\x00\x15\x00\x09\x00\x56\x00\x3f\x00\x19\x00\x1a\x00\x1b\x00\x0b\x00\x15\x00\x05\x00\x1f\x00\x59\x00\x21\x00\x12\x00\x23\x00\x12\x00\x20\x00\x15\x00\x1f\x00\x40\x00\x21\x00\x19\x00\x1a\x00\x1b\x00\x35\x00\x03\x00\x04\x00\x1f\x00\x3f\x00\x21\x00\x78\x00\x23\x00\x13\x00\x14\x00\x7c\x00\x13\x00\x14\x00\x03\x00\x04\x00\x23\x00\x24\x00\x7d\x00\x3f\x00\x03\x00\x04\x00\x78\x00\x7c\x00\x3d\x00\x15\x00\x7c\x00\x3f\x00\x14\x00\x58\x00\x59\x00\x1b\x00\x7c\x00\x1c\x00\x07\x00\x1f\x00\x3f\x00\x21\x00\x43\x00\x23\x00\x31\x00\x7c\x00\x7e\x00\x63\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x7d\x00\x64\x00\x2d\x00\x31\x00\x7d\x00\x7d\x00\x7c\x00\x7c\x00\x2e\x00\x71\x00\x7d\x00\x73\x00\x3d\x00\x70\x00\x71\x00\x72\x00\x73\x00\x7c\x00\x7c\x00\x3f\x00\x7c\x00\x64\x00\x18\x00\x63\x00\x80\x00\x7c\x00\x30\x00\x31\x00\x7d\x00\x7d\x00\x7c\x00\x82\x00\x7c\x00\x70\x00\x71\x00\x72\x00\x73\x00\x71\x00\x7d\x00\x73\x00\x7c\x00\x63\x00\x17\x00\x65\x00\x63\x00\x7c\x00\x65\x00\x7c\x00\x7c\x00\x7c\x00\x57\x00\x82\x00\x80\x00\x22\x00\x7e\x00\x71\x00\x57\x00\x73\x00\x71\x00\x63\x00\x73\x00\x65\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x7c\x00\x7c\x00\x2d\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x71\x00\x5e\x00\x73\x00\x73\x00\x73\x00\x7c\x00\x22\x00\x7c\x00\x7c\x00\x15\x00\x7c\x00\x7c\x00\x7c\x00\x7c\x00\x1a\x00\x5c\x00\x84\x00\x47\x00\x81\x00\x49\x00\x4a\x00\x03\x00\x04\x00\x4d\x00\x4e\x00\x4f\x00\x36\x00\x37\x00\x67\x00\x7c\x00\x50\x00\x6a\x00\x7d\x00\x6c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x0d\x00\x0e\x00\x75\x00\x63\x00\x62\x00\x78\x00\x64\x00\x7c\x00\x66\x00\x7c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x7e\x00\x71\x00\x70\x00\x71\x00\x72\x00\x73\x00\x0f\x00\x71\x00\x72\x00\x73\x00\x0e\x00\x75\x00\x7c\x00\x7b\x00\x7c\x00\x1d\x00\x64\x00\x15\x00\x7c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x3d\x00\x35\x00\x2d\x00\x84\x00\x29\x00\x70\x00\x71\x00\x0e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x15\x00\x2f\x00\x39\x00\x7c\x00\x0d\x00\x0e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x15\x00\x03\x00\x04\x00\x71\x00\x72\x00\x73\x00\x0e\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x50\x00\x22\x00\x15\x00\x7c\x00\x03\x00\x04\x00\x03\x00\x04\x00\x71\x00\x72\x00\x73\x00\x84\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x71\x00\x7c\x00\x73\x00\x35\x00\x37\x00\x05\x00\x06\x00\x07\x00\x08\x00\x84\x00\x2e\x00\x7c\x00\x1d\x00\x25\x00\x1d\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x3c\x00\x7f\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x84\x00\x23\x00\x24\x00\x39\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x84\x00\x38\x00\x7d\x00\x64\x00\x7c\x00\x0d\x00\x71\x00\x72\x00\x73\x00\x5a\x00\x5b\x00\x5c\x00\x84\x00\x05\x00\x06\x00\x70\x00\x71\x00\x7c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x71\x00\x72\x00\x73\x00\x84\x00\x24\x00\x7c\x00\x60\x00\x61\x00\x0d\x00\x0e\x00\x64\x00\x7c\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x15\x00\x25\x00\x3a\x00\x84\x00\x7e\x00\x7c\x00\x70\x00\x71\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x05\x00\x06\x00\x7c\x00\x7c\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x84\x00\x7c\x00\x0d\x00\x0e\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x84\x00\x18\x00\x71\x00\x40\x00\x7c\x00\x7c\x00\x23\x00\x24\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x84\x00\x7c\x00\x3b\x00\x71\x00\x72\x00\x73\x00\x0e\x00\x72\x00\x73\x00\x84\x00\x28\x00\x29\x00\x2a\x00\x15\x00\x7c\x00\x03\x00\x04\x00\x7c\x00\x0e\x00\x71\x00\x72\x00\x73\x00\x84\x00\x35\x00\x83\x00\x15\x00\x29\x00\x2a\x00\x0d\x00\x0e\x00\x7c\x00\x28\x00\x29\x00\x2a\x00\x71\x00\x72\x00\x73\x00\x34\x00\x84\x00\x29\x00\x2a\x00\x32\x00\x33\x00\x7f\x00\x35\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x05\x00\x06\x00\x07\x00\x08\x00\x84\x00\x29\x00\x2a\x00\x3c\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x29\x00\x2a\x00\x15\x00\x6a\x00\x84\x00\x29\x00\x2a\x00\x1a\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x1d\x00\x20\x00\x75\x00\x2f\x00\x84\x00\x0c\x00\x23\x00\x24\x00\x7c\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x29\x00\x2a\x00\x0e\x00\x84\x00\x29\x00\x2a\x00\x65\x00\x16\x00\x7c\x00\x15\x00\x0d\x00\x0e\x00\x71\x00\x72\x00\x73\x00\x05\x00\x84\x00\x07\x00\x71\x00\x72\x00\x73\x00\x18\x00\x17\x00\x7c\x00\x19\x00\x71\x00\x72\x00\x73\x00\x1d\x00\x7c\x00\x1f\x00\x84\x00\x0d\x00\x0d\x00\x03\x00\x04\x00\x7c\x00\x84\x00\x13\x00\x13\x00\x71\x00\x72\x00\x73\x00\x0e\x00\x84\x00\x23\x00\x24\x00\x71\x00\x72\x00\x73\x00\x15\x00\x7c\x00\x71\x00\x72\x00\x73\x00\x24\x00\x24\x00\x0e\x00\x7c\x00\x84\x00\x01\x00\x02\x00\x0e\x00\x7c\x00\x15\x00\x61\x00\x84\x00\x15\x00\x64\x00\x15\x00\x18\x00\x84\x00\x71\x00\x72\x00\x73\x00\x1d\x00\x71\x00\x72\x00\x73\x00\x0d\x00\x70\x00\x71\x00\x0d\x00\x7c\x00\x7f\x00\x13\x00\x75\x00\x7c\x00\x13\x00\x78\x00\x07\x00\x84\x00\x7c\x00\x7c\x00\x0d\x00\x84\x00\x01\x00\x02\x00\x03\x00\x04\x00\x13\x00\x14\x00\x24\x00\x7e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x76\x00\x0f\x00\x78\x00\x0d\x00\x7a\x00\x13\x00\x7c\x00\x11\x00\x16\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x11\x00\x03\x00\x04\x00\x21\x00\x15\x00\x23\x00\x7f\x00\x25\x00\x26\x00\x27\x00\x28\x00\x0d\x00\x2a\x00\x7c\x00\x2c\x00\x1f\x00\x2e\x00\x13\x00\x30\x00\x31\x00\x32\x00\x33\x00\x0d\x00\x0e\x00\x36\x00\x03\x00\x04\x00\x39\x00\x13\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x70\x00\x71\x00\x72\x00\x73\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x17\x00\x15\x00\x19\x00\x7c\x00\x1b\x00\x13\x00\x1a\x00\x0e\x00\x16\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x1d\x00\x03\x00\x04\x00\x21\x00\x06\x00\x23\x00\x3a\x00\x25\x00\x26\x00\x27\x00\x28\x00\x11\x00\x2a\x00\x0d\x00\x2c\x00\x15\x00\x2e\x00\x11\x00\x30\x00\x31\x00\x32\x00\x33\x00\x03\x00\x04\x00\x36\x00\x03\x00\x04\x00\x39\x00\x1d\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x70\x00\x71\x00\x72\x00\x73\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x02\x00\x03\x00\x04\x00\x7c\x00\x0e\x00\x13\x00\x03\x00\x04\x00\x16\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x21\x00\x0e\x00\x23\x00\x0d\x00\x25\x00\x26\x00\x27\x00\x28\x00\x15\x00\x2a\x00\x17\x00\x2c\x00\x19\x00\x1a\x00\x0f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x3f\x00\x40\x00\x36\x00\x03\x00\x04\x00\x39\x00\x0d\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0e\x00\x0f\x00\x10\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0f\x00\x0f\x00\x02\x00\x03\x00\x04\x00\x13\x00\x0e\x00\x1f\x00\x16\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x1a\x00\x0f\x00\x0e\x00\x0e\x00\x21\x00\x0e\x00\x23\x00\x0f\x00\x25\x00\x0d\x00\x0e\x00\x06\x00\x11\x00\x2a\x00\x17\x00\x13\x00\x19\x00\x15\x00\x1b\x00\x30\x00\x31\x00\x32\x00\x1f\x00\x01\x00\x10\x00\x03\x00\x04\x00\x1f\x00\x10\x00\x17\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0d\x00\x0d\x00\x01\x00\x1a\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0d\x00\x0f\x00\x11\x00\x72\x00\x73\x00\x13\x00\x0f\x00\x2b\x00\x16\x00\x05\x00\x06\x00\x07\x00\x08\x00\x7c\x00\x1c\x00\x01\x00\x2f\x00\x03\x00\x1e\x00\x21\x00\x83\x00\x23\x00\x0d\x00\x25\x00\x0e\x00\x17\x00\x17\x00\x17\x00\x2a\x00\x19\x00\x1a\x00\x2d\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x34\x00\x23\x00\x24\x00\x17\x00\x1b\x00\x01\x00\x0e\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x18\x00\x0d\x00\x10\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x11\x00\x67\x00\x1e\x00\x0d\x00\x6a\x00\x13\x00\x6c\x00\x0c\x00\x16\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x75\x00\x72\x00\x73\x00\x78\x00\x21\x00\x1b\x00\x23\x00\x7c\x00\x25\x00\x0e\x00\x29\x00\x7c\x00\x14\x00\x2a\x00\x41\x00\x42\x00\x43\x00\x10\x00\x83\x00\x30\x00\x31\x00\x32\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x0c\x00\x3b\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x10\x00\x0f\x00\x11\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x15\x00\x0f\x00\x0e\x00\x10\x00\x68\x00\x13\x00\x10\x00\x6b\x00\x16\x00\x6d\x00\x38\x00\x6f\x00\x2f\x00\x0e\x00\x0e\x00\x0e\x00\x74\x00\x75\x00\x76\x00\x21\x00\x78\x00\x23\x00\x7a\x00\x25\x00\x7c\x00\x17\x00\x10\x00\x1d\x00\x2a\x00\x17\x00\x1b\x00\x17\x00\x15\x00\x0e\x00\x0e\x00\x17\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x08\x00\x0d\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x1d\x00\x0e\x00\x0e\x00\x0e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x13\x00\x23\x00\x24\x00\x16\x00\x3a\x00\x13\x00\x15\x00\x14\x00\x16\x00\x10\x00\x20\x00\x37\x00\x3a\x00\x01\x00\x21\x00\x1b\x00\x23\x00\x1f\x00\x25\x00\x21\x00\x01\x00\x3a\x00\x1b\x00\x2a\x00\x22\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x03\x00\x1b\x00\x0f\x00\x3e\x00\x0d\x00\x10\x00\x10\x00\x09\x00\x10\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x01\x00\x02\x00\x03\x00\x04\x00\x0d\x00\x15\x00\x0e\x00\x11\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x3a\x00\x0d\x00\x41\x00\x35\x00\x01\x00\x13\x00\x03\x00\x04\x00\x16\x00\x06\x00\xff\xff\x01\x00\xff\xff\x03\x00\x04\x00\xff\xff\x0d\x00\xff\xff\x01\x00\x21\x00\x03\x00\x04\x00\x13\x00\x0d\x00\xff\xff\x01\x00\x17\x00\x03\x00\x04\x00\x13\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\x0d\x00\xff\xff\x24\x00\xff\xff\x1f\x00\xff\xff\x13\x00\x22\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\xff\xff\x24\x00\xff\xff\x1f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x50\x00\x51\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x69\x00\x21\x00\x6b\x00\xff\xff\xff\xff\x6e\x00\x6f\x00\x70\x00\x71\x00\x72\x00\x73\x00\x74\x00\x75\x00\x76\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x7c\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\x68\x00\x54\x00\x55\x00\x6b\x00\xff\xff\x6d\x00\xff\xff\x6f\x00\xff\xff\xff\xff\x5d\x00\xff\xff\x74\x00\x75\x00\x76\x00\x62\x00\x78\x00\x64\x00\x7a\x00\x66\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\x47\x00\x66\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\x55\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\xff\xff\x5f\x00\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x52\x00\x53\x00\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\x45\x00\x46\x00\x47\x00\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x47\x00\xff\xff\x49\x00\x4a\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x5d\x00\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\xff\xff\xff\xff\x48\x00\x49\x00\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\x4a\x00\x7b\x00\x7c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\x4b\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\xff\xff\x2c\x00\xff\xff\x2e\x00\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\x36\x00\xff\xff\xff\xff\x39\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\x26\x00\x27\x00\x28\x00\xff\xff\x2a\x00\xff\xff\x2c\x00\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\xff\xff\xff\xff\x36\x00\xff\xff\xff\xff\x39\x00\x4c\x00\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\x4d\x00\x4e\x00\x4f\x00\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\x4d\x00\x4e\x00\x4f\x00\xff\xff\xff\xff\x4d\x00\x4e\x00\x4f\x00\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\x62\x00\xff\xff\x64\x00\xff\xff\x66\x00\xff\xff\xff\xff\xff\xff\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\x70\x00\x71\x00\x72\x00\x73\x00\xff\xff\xff\xff\x7b\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x7b\x00\x7c\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\xff\xff\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\x15\x00\x16\x00\x17\x00\xff\xff\x19\x00\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\x05\x00\x06\x00\x07\x00\x08\x00\x2a\x00\xff\xff\xff\xff\x2d\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x34\x00\x0f\x00\xff\xff\xff\xff\x17\x00\x13\x00\x19\x00\x1a\x00\x16\x00\xff\xff\xff\xff\x1e\x00\xff\xff\xff\xff\x1c\x00\xff\xff\x23\x00\x24\x00\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\x68\x00\x13\x00\x14\x00\x6b\x00\x16\x00\x6d\x00\xff\xff\x6f\x00\xff\xff\xff\xff\x1c\x00\xff\xff\x74\x00\x75\x00\x76\x00\x21\x00\x78\x00\x23\x00\x7a\x00\x25\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x34\x00\x0f\x00\xff\xff\xff\xff\x68\x00\x13\x00\xff\xff\x6b\x00\x16\x00\x6d\x00\xff\xff\x6f\x00\xff\xff\xff\xff\x1c\x00\xff\xff\x74\x00\x75\x00\x76\x00\x21\x00\x78\x00\x23\x00\x7a\x00\x25\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\x68\x00\x13\x00\x14\x00\x6b\x00\x16\x00\x6d\x00\xff\xff\x6f\x00\xff\xff\xff\xff\x1c\x00\xff\xff\x74\x00\x75\x00\x76\x00\x21\x00\x78\x00\x23\x00\x7a\x00\x25\x00\x7c\x00\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x34\x00\x0f\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\x14\x00\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1c\x00\xff\xff\xff\xff\xff\xff\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\x2d\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\xff\xff\x34\x00\xff\xff\xff\xff\xff\xff\x13\x00\xff\xff\xff\xff\x16\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\x13\x00\xff\xff\xff\xff\x16\x00\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\xff\xff\x21\x00\xff\xff\x23\x00\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\xff\xff\x2a\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\xde\x01\xad\x00\x05\x00\x03\x00\x7c\x01\x23\x00\x24\x00\x25\x00\x3e\x01\x46\x01\xdf\x01\xe0\x01\x15\x00\x08\x00\x64\x01\x13\x00\x14\x00\xb8\x01\x91\x01\xb4\x01\xb5\x01\x82\x01\x83\x01\x12\x00\x1a\x00\xd9\xff\x15\x00\xbe\x00\x47\x01\xf2\x01\x16\x00\x17\x00\x18\x00\x58\x00\x15\x00\xcb\x00\x19\x00\xfb\x01\x1a\x00\x06\x02\x1b\x00\x13\x00\x00\x01\x15\x00\xd1\x01\x04\x02\x1a\x00\x59\x00\x17\x00\x18\x00\x09\x00\x0d\x00\x9a\x01\x19\x00\x1c\x00\x1a\x00\xb9\x01\x1b\x00\x08\x02\xc1\x00\x69\x01\xc0\x00\xc1\x00\x0d\x00\x88\x01\xcd\x00\xce\x00\x48\x01\x1c\x00\x0d\x00\x88\x01\x68\x01\xc7\x01\x9a\x01\x15\x00\x69\x01\x1c\x00\x71\x01\xc5\x01\xc6\x01\x5a\x00\xb6\x01\x0a\x01\xf4\x01\x19\x00\x1c\x00\x1a\x00\xe1\x00\x1b\x00\xc2\x01\xb6\x01\xb2\x01\xe1\x01\x0b\x01\x0c\x01\x62\x00\x63\x00\x01\x01\x26\x00\x0d\x01\x2a\x01\x01\x01\x01\x01\x92\x01\x92\x01\x5c\x00\xc4\x00\x01\x01\xe2\x01\x9b\x01\x27\x00\x28\x00\x29\x00\x2a\x00\xc7\x01\x84\x01\x1c\x00\xc6\x00\x26\x00\xce\x00\xe1\x01\xe3\x01\x2b\x00\x44\x01\x45\x01\x06\x00\x06\x00\x04\x00\x2c\x00\x1d\x00\x27\x00\x28\x00\x29\x00\x2a\x00\xc4\x00\x01\x01\xe2\x01\x84\x01\xc2\x00\x31\x00\xc3\x00\xc2\x00\x2b\x00\xc3\x00\xe2\x00\xc6\x00\x1d\x00\x89\x01\x2c\x00\xe3\x01\x32\x00\xc3\x01\xc4\x00\x8a\x01\xc5\x00\xc4\x00\xc2\x00\xc5\x00\xc3\x00\x1c\x01\x0c\x01\x62\x00\x63\x00\xc6\x00\x2b\x01\x1d\x01\xc6\x00\x66\x00\x67\x00\x2a\x00\xc4\x00\x71\x00\xc5\x00\x6b\x01\x5d\x00\x2b\x01\xea\x01\x5b\x00\x68\x00\xaa\x00\x7d\x01\xc6\x00\x5e\x00\x5e\x00\xf2\x01\x03\x02\x69\x00\x33\x00\x5f\x00\x34\x00\x35\x00\x0d\x00\x9a\x01\x36\x00\x37\x00\x38\x00\xeb\x01\xec\x01\xcf\x00\x2b\x01\x1e\x01\xd0\x00\x72\x00\xd1\x00\x60\x00\x61\x00\x62\x00\x63\x00\x7a\x01\x65\x00\x2e\x00\xef\xff\xd2\x00\xd7\x00\x39\x00\xd3\x00\x3a\x00\xff\x01\x3b\x00\xd4\x00\x60\x00\x61\x00\x62\x00\x63\x00\xd6\x00\x65\x00\x80\x01\xc4\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x91\x01\x66\x00\x67\x00\x2a\x00\x12\x02\x6e\x01\xd8\x00\x3d\x00\x2b\x00\x8d\x01\xed\x01\x71\x01\xa9\x01\x1c\x01\x0c\x01\x62\x00\x63\x00\x9c\x01\x30\x00\x1d\x01\x69\x00\x8e\x01\x27\x00\x28\x00\xfa\x01\x60\x00\x61\x00\x62\x00\x63\x00\x64\x00\x65\x00\xfb\x01\x53\x01\x8b\x01\xe8\x00\x2e\x00\xee\xff\x60\x00\x61\x00\x62\x00\x63\x00\x6c\x00\x65\x00\x2f\x00\xd5\x01\xd6\x01\x66\x00\x67\x00\x2a\x00\x2f\x01\xf7\x01\x0c\x01\x62\x00\x63\x00\x1e\x01\xea\x01\xf6\x00\x68\x00\x0d\x00\x4f\x01\x0d\x00\x4f\x01\x66\x00\x67\x00\x2a\x00\x69\x00\x60\x00\x61\x00\x62\x00\x63\x00\x75\x00\x65\x00\x6a\x01\x68\x00\x6b\x01\x30\x00\x0e\x02\xcb\x00\xde\x00\xcc\x00\xdf\x00\x69\x00\x70\x00\xc6\x00\x50\x01\x94\x01\x51\x01\x66\x00\x67\x00\x2a\x00\x60\x00\x61\x00\x62\x00\x63\x00\x76\x00\x65\x00\x71\x00\x95\x01\x68\x00\x66\x00\x67\x00\x2a\x00\x0b\x02\x0c\x01\x62\x00\x63\x00\x69\x00\xcd\x00\xce\x00\xa7\x01\x68\x00\x66\x00\x67\x00\x2a\x00\xdb\x01\x0c\x01\x62\x00\x63\x00\x69\x00\x0c\x02\x34\x01\xed\x01\x68\x00\x36\x01\x66\x00\x67\x00\x2a\x00\xfc\x01\xfd\x01\xfe\x01\x69\x00\x20\x00\x21\x00\x27\x00\x28\x00\x68\x00\x9d\x01\x0c\x01\x62\x00\x63\x00\x66\x00\x67\x00\x2a\x00\x69\x00\x38\x01\xe8\x00\xe5\x00\xe6\x00\xda\x00\xee\xff\xe7\x00\x68\x00\x9e\x01\x0c\x01\x62\x00\x63\x00\x2f\x00\x3b\x01\xd7\x01\x69\x00\x3d\x01\xff\x01\x27\x00\x28\x00\x66\x00\x67\x00\x2a\x00\x27\x01\x0c\x01\x62\x00\x63\x00\xcb\x00\xde\x00\x41\x01\xe8\x00\x68\x00\x66\x00\x67\x00\x2a\x00\x29\x01\x0c\x01\x62\x00\x63\x00\x69\x00\x5c\x01\xc8\x00\x0a\x02\x68\x00\x66\x00\x67\x00\x2a\x00\x2c\x01\x0c\x01\x62\x00\x63\x00\x69\x00\x0b\x02\x66\x00\xe3\x00\x68\x00\x04\x01\xcd\x00\xce\x00\x1a\x01\x0c\x01\x62\x00\x63\x00\x69\x00\xd8\x00\x07\x01\x66\x00\x67\x00\x2a\x00\x30\x01\xbe\x01\x2a\x00\x28\x01\xe8\x01\x62\x00\x63\x00\x31\x01\x68\x00\xe4\x01\xe5\x01\xbf\x01\x32\x01\x66\x00\x67\x00\x2a\x00\x69\x00\xe9\x01\xd8\x01\x33\x01\xae\x01\x63\x00\xda\x00\xef\xff\x68\x00\x9f\x01\x62\x00\x63\x00\x66\x00\x67\x00\x2a\x00\xaf\x01\x69\x00\x0f\x02\x63\x00\xa0\x01\xa1\x01\x17\x01\xa2\x01\x68\x00\x66\x00\x67\x00\x2a\x00\xcb\x00\xde\x00\xcc\x00\xdf\x00\x69\x00\x23\x01\x63\x00\x18\x01\x68\x00\x66\x00\x67\x00\x2a\x00\xe6\x01\x63\x00\xac\xff\xab\x01\x69\x00\xe7\x01\x63\x00\xac\xff\x68\x00\x66\x00\x67\x00\x2a\x00\xe0\x00\x9b\x00\xd2\x00\x25\x01\x69\x00\x6d\x00\xcd\x00\xce\x00\x68\x00\xac\x01\x66\x00\x67\x00\x2a\x00\xa8\x01\x63\x00\x70\x01\x69\x00\x23\x01\x63\x00\xa3\x01\xab\x00\x68\x00\x71\x01\xc8\x00\xc9\x00\x66\x00\x67\x00\x2a\x00\xcb\x00\x69\x00\xcc\x00\x66\x00\x67\x00\xa4\x01\xca\x00\xa8\x00\x68\x00\x9a\x00\x66\x00\x67\x00\x2a\x00\x22\xff\x68\x00\x22\xff\x69\x00\x6b\x00\x6b\x00\xca\x01\xcb\x01\x68\x00\x69\x00\x6c\x00\x6c\x00\x66\x00\x67\x00\x2a\x00\xf3\x00\x69\x00\xcd\x00\xce\x00\x66\x00\x67\x00\x2a\x00\xf4\x00\x68\x00\x66\x00\x67\x00\x2a\x00\x0e\x02\xa7\x01\xf5\x00\x68\x00\x69\x00\xb2\x00\xb3\x00\xfa\x00\x68\x00\xf6\x00\x78\x01\x69\x00\xfe\x00\xe7\x00\xfb\x00\xff\x00\x69\x00\x66\x00\x67\x00\x2a\x00\x00\x01\x66\x00\x67\x00\x2a\x00\xa6\x01\x27\x00\x28\x00\x6b\x00\x68\x00\xae\x00\x6c\x00\x6e\x01\x68\x00\x6c\x00\x68\x01\xbb\x00\x69\x00\xe8\x00\xd4\x00\x6b\x00\x69\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x6c\x00\x1c\x01\xa7\x01\x10\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x90\x00\x1f\x00\x92\x00\x2e\x00\xa5\x00\xdb\xfe\xaf\x00\xef\x01\xdb\xfe\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xdb\xfe\xf0\x01\xdb\xfe\xdb\xfe\xdb\xfe\xf1\x01\xdb\xfe\x09\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x6b\x00\xdb\xfe\x02\x00\xdb\xfe\x14\x02\xdb\xfe\x6c\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x6b\x00\xb8\xff\xdb\xfe\x54\x00\x55\x00\xdb\xfe\x6c\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\x00\x28\x00\xdc\x00\x2a\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xa8\x00\xaa\x00\x9a\x00\x2b\x00\xa9\x00\xdb\xfe\xab\x00\x11\x02\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xb2\x00\x03\x02\xb4\x00\xb5\x00\xdb\xfe\xde\x00\xdb\xfe\xe5\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x63\x01\xdb\xfe\x2e\x00\xdb\xfe\x64\x01\xdb\xfe\xea\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x56\x00\x57\x00\xdb\xfe\xb4\x00\xb5\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xeb\x00\x28\x00\xdc\x00\x2a\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x0c\x00\x0d\x00\x0e\x00\x2b\x00\x08\x02\xdb\xfe\x0b\x00\x0c\x00\xdb\xfe\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xb2\x00\xb3\x00\xb4\x00\xb5\x00\xdb\xfe\x5f\xff\xdb\xfe\x2e\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x5f\xff\xdb\xfe\xa8\x00\xdb\xfe\x9a\x00\xf9\x00\x1f\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x3a\x01\x3b\x01\xdb\xfe\x0b\x00\x0c\x00\xdb\xfe\xf4\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x96\x01\x97\x01\x98\x01\xf6\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x1f\x00\x1f\x00\x0f\x00\x0d\x00\x0e\x00\xdb\xfe\xf7\x01\x01\x02\xdb\xfe\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xde\x01\x1f\x00\xda\x01\xdd\x01\xdb\xfe\x5f\xff\xdb\xfe\xdb\x01\xdb\xfe\x6b\x00\x20\x01\xde\x00\xb4\x01\xdb\xfe\xa8\x00\x6c\x00\x9a\x00\x98\x00\x5f\xff\xdb\xfe\xdb\xfe\xdb\xfe\x5f\xff\xb2\x00\x0e\xff\xb4\x00\xb5\x00\x21\x01\xab\x01\xae\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xb8\x01\x94\x01\xbb\x01\xbc\x01\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\xc2\x01\x13\x01\xc5\x01\xbe\x01\x2a\x00\x44\x00\x1f\x00\xca\x01\x45\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xbf\x01\x7e\x00\xb2\x00\x53\x01\xb4\x00\xce\x01\x46\x00\x05\x02\x47\x00\xc8\x00\x48\x00\x73\x01\xd6\x00\x75\x01\xa8\x00\x4c\x00\x9a\x00\xf9\x00\x7f\x00\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\x14\x01\xdb\xfe\xdb\xfe\x76\x01\x78\x01\xb2\x00\x77\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x7c\x01\x2e\x00\x03\x01\x7f\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x82\x01\xcf\x00\x90\x01\x94\x01\xd0\x00\xdb\xfe\x73\x01\x3d\x01\xdb\xfe\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xd2\x00\xbe\x01\x2a\x00\xd3\x00\xdb\xfe\x8f\xff\xdb\xfe\xd4\x00\xdb\xfe\x2e\x01\x8e\x01\xbf\x01\x34\x01\xdb\xfe\x9d\x00\x9e\x00\x9f\x00\x36\x01\xc0\x01\xdb\xfe\xdb\xfe\xdb\xfe\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\x3d\x01\x38\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x03\x01\x43\x01\x41\x01\x31\x01\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x33\x01\x1f\x00\x44\x01\x03\x01\xa0\x00\xdb\xfe\x4a\x01\xa1\x00\xdb\xfe\xa2\x00\x4f\x01\xa3\x00\x53\x01\x60\x01\xe3\xfe\xe5\xfe\x8e\x00\x8f\x00\xa4\x00\xdb\xfe\x92\x00\xdb\xfe\xa5\x00\xdb\xfe\xa6\x00\x61\x01\x03\x01\xe0\x00\xdb\xfe\x67\x01\x62\x01\x68\x01\x6d\x01\x6e\x01\xc0\x00\xd6\x00\xb2\x00\xb3\x00\xb4\x00\xb5\x00\xed\x00\xde\x00\xcc\x00\xdf\x00\xda\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xe8\xfe\xef\x00\xf0\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\xdb\xfe\xcd\x00\xee\x00\xdb\xfe\xe5\x00\x44\x00\xfc\x00\xfd\x00\x45\x00\x03\x01\x9b\x00\x07\x01\x09\x01\x16\x01\xdb\xfe\x0a\x01\xdb\xfe\x88\x01\xdb\xfe\x46\x00\x17\x01\x1a\x01\x23\x01\xdb\xfe\x22\x01\xb6\x00\xb7\x00\xb8\x00\xb9\x00\xba\x00\xbb\x00\xb4\x00\x27\x01\x1f\x00\x6f\x00\x75\x00\x74\x00\x9c\x00\xad\x00\x08\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xb1\x00\xbd\x00\xbe\x00\x20\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x31\x00\x23\x00\xff\xff\x09\x00\xdb\xfe\x44\x00\xdb\xfe\xdb\xfe\x45\x00\x80\xff\x00\x00\xdb\xfe\x00\x00\xdb\xfe\xdb\xfe\x00\x00\x6b\x00\x00\x00\xdb\xfe\x46\x00\xdb\xfe\xdb\xfe\x6c\x00\x6b\x00\x00\x00\xdb\xfe\x80\xff\xdb\xfe\xdb\xfe\x6c\x00\x6b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x00\x6b\x00\x00\x00\xb2\x01\x00\x00\x25\x01\x00\x00\x6c\x00\x92\xff\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\x00\x00\xb1\x01\x00\x00\x25\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x00\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x86\x00\x78\x00\x79\x00\x87\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x88\x00\x89\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x8a\x00\x46\x00\x8b\x00\x00\x00\x00\x00\x8c\x00\x8d\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x8e\x00\x8f\x00\x90\x00\x91\x00\x92\x00\x93\x00\x94\x00\x3d\x00\x95\x00\x53\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x54\x01\x55\x01\xa1\x00\x00\x00\xa2\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x56\x01\x00\x00\x8e\x00\x8f\x00\xa4\x00\x39\x00\x92\x00\x7c\x00\xa5\x00\x3b\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\xf8\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\xbc\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x33\x00\x3b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x53\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\xcc\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x56\x01\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x8e\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x3f\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x11\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x0e\x01\x78\x00\x79\x00\x0f\x01\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x01\x00\x00\x14\x01\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x82\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x83\x00\x84\x00\x00\x00\x00\x00\x14\x02\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x01\x02\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd0\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\xd2\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xbd\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\xc8\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xce\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\xcf\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x7f\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x58\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x5a\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x5e\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xda\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x03\x01\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x77\x00\x78\x00\x79\x00\x7a\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x12\x02\x79\x00\xf1\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x65\x01\x79\x00\xf1\x00\x7b\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\xf0\x00\x79\x00\xf1\x00\x7b\x00\x34\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd3\x01\x00\x00\x34\x00\x35\x00\x00\x00\x00\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd4\x01\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\xe0\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4a\x01\xe0\x00\x35\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x80\x00\x3d\x00\x2b\x00\x36\x00\x37\x00\x38\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x4b\x01\x4c\x01\x4d\x01\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x4e\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x54\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x00\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x52\x00\x00\x00\x00\x00\x53\x00\x00\x00\x00\x00\x54\x00\x86\x01\x4d\x01\x37\x00\x38\x00\x00\x00\x00\x00\xea\x00\x37\x00\x38\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x9c\x00\x37\x00\x38\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x81\x00\x37\x00\x38\x00\x00\x00\x00\x00\x9c\x00\x37\x00\x38\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x39\x00\x00\x00\x7c\x00\x00\x00\x3b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x27\x00\x28\x00\x3c\x00\x2a\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x3d\x00\x2b\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x97\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x98\x00\x45\x00\x99\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\xdb\xfe\xdb\xfe\xdb\xfe\xdb\xfe\x4c\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x80\x00\x13\x01\x00\x00\x00\x00\xa8\x00\x44\x00\x9a\x00\xf9\x00\x45\x00\x00\x00\x00\x00\x22\xff\x00\x00\x00\x00\x7e\x00\x00\x00\xdb\xfe\xdb\xfe\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x14\x01\x00\x00\x00\x00\xa0\x00\x44\x00\x38\xff\xa1\x00\x45\x00\x05\x01\x00\x00\xa3\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x8e\x00\x8f\x00\xa4\x00\x46\x00\x92\x00\x47\x00\xa5\x00\x48\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x80\x00\x13\x01\x00\x00\x00\x00\xa0\x00\x44\x00\x00\x00\xa1\x00\x45\x00\xf6\x00\x00\x00\xa3\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x8e\x00\x8f\x00\xa4\x00\x46\x00\x92\x00\x47\x00\xa5\x00\x48\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x14\x01\x00\x00\x00\x00\xa0\x00\x44\x00\x39\xff\xa1\x00\x45\x00\x05\x01\x00\x00\xa3\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x8e\x00\x8f\x00\xa4\x00\x46\x00\x92\x00\x47\x00\xa5\x00\x48\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x5e\x01\x80\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x80\x00\x13\x01\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x14\x01\x00\x00\x00\x00\x00\x00\x44\x00\x86\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x58\x01\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x58\x01\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x7f\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x45\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x44\x00\x00\x00\x00\x00\x45\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x50\x00\x51\x00\x00\x00\x46\x00\x00\x00\x47\x00\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = array (1, 302) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151),
	(152 , happyReduce_152),
	(153 , happyReduce_153),
	(154 , happyReduce_154),
	(155 , happyReduce_155),
	(156 , happyReduce_156),
	(157 , happyReduce_157),
	(158 , happyReduce_158),
	(159 , happyReduce_159),
	(160 , happyReduce_160),
	(161 , happyReduce_161),
	(162 , happyReduce_162),
	(163 , happyReduce_163),
	(164 , happyReduce_164),
	(165 , happyReduce_165),
	(166 , happyReduce_166),
	(167 , happyReduce_167),
	(168 , happyReduce_168),
	(169 , happyReduce_169),
	(170 , happyReduce_170),
	(171 , happyReduce_171),
	(172 , happyReduce_172),
	(173 , happyReduce_173),
	(174 , happyReduce_174),
	(175 , happyReduce_175),
	(176 , happyReduce_176),
	(177 , happyReduce_177),
	(178 , happyReduce_178),
	(179 , happyReduce_179),
	(180 , happyReduce_180),
	(181 , happyReduce_181),
	(182 , happyReduce_182),
	(183 , happyReduce_183),
	(184 , happyReduce_184),
	(185 , happyReduce_185),
	(186 , happyReduce_186),
	(187 , happyReduce_187),
	(188 , happyReduce_188),
	(189 , happyReduce_189),
	(190 , happyReduce_190),
	(191 , happyReduce_191),
	(192 , happyReduce_192),
	(193 , happyReduce_193),
	(194 , happyReduce_194),
	(195 , happyReduce_195),
	(196 , happyReduce_196),
	(197 , happyReduce_197),
	(198 , happyReduce_198),
	(199 , happyReduce_199),
	(200 , happyReduce_200),
	(201 , happyReduce_201),
	(202 , happyReduce_202),
	(203 , happyReduce_203),
	(204 , happyReduce_204),
	(205 , happyReduce_205),
	(206 , happyReduce_206),
	(207 , happyReduce_207),
	(208 , happyReduce_208),
	(209 , happyReduce_209),
	(210 , happyReduce_210),
	(211 , happyReduce_211),
	(212 , happyReduce_212),
	(213 , happyReduce_213),
	(214 , happyReduce_214),
	(215 , happyReduce_215),
	(216 , happyReduce_216),
	(217 , happyReduce_217),
	(218 , happyReduce_218),
	(219 , happyReduce_219),
	(220 , happyReduce_220),
	(221 , happyReduce_221),
	(222 , happyReduce_222),
	(223 , happyReduce_223),
	(224 , happyReduce_224),
	(225 , happyReduce_225),
	(226 , happyReduce_226),
	(227 , happyReduce_227),
	(228 , happyReduce_228),
	(229 , happyReduce_229),
	(230 , happyReduce_230),
	(231 , happyReduce_231),
	(232 , happyReduce_232),
	(233 , happyReduce_233),
	(234 , happyReduce_234),
	(235 , happyReduce_235),
	(236 , happyReduce_236),
	(237 , happyReduce_237),
	(238 , happyReduce_238),
	(239 , happyReduce_239),
	(240 , happyReduce_240),
	(241 , happyReduce_241),
	(242 , happyReduce_242),
	(243 , happyReduce_243),
	(244 , happyReduce_244),
	(245 , happyReduce_245),
	(246 , happyReduce_246),
	(247 , happyReduce_247),
	(248 , happyReduce_248),
	(249 , happyReduce_249),
	(250 , happyReduce_250),
	(251 , happyReduce_251),
	(252 , happyReduce_252),
	(253 , happyReduce_253),
	(254 , happyReduce_254),
	(255 , happyReduce_255),
	(256 , happyReduce_256),
	(257 , happyReduce_257),
	(258 , happyReduce_258),
	(259 , happyReduce_259),
	(260 , happyReduce_260),
	(261 , happyReduce_261),
	(262 , happyReduce_262),
	(263 , happyReduce_263),
	(264 , happyReduce_264),
	(265 , happyReduce_265),
	(266 , happyReduce_266),
	(267 , happyReduce_267),
	(268 , happyReduce_268),
	(269 , happyReduce_269),
	(270 , happyReduce_270),
	(271 , happyReduce_271),
	(272 , happyReduce_272),
	(273 , happyReduce_273),
	(274 , happyReduce_274),
	(275 , happyReduce_275),
	(276 , happyReduce_276),
	(277 , happyReduce_277),
	(278 , happyReduce_278),
	(279 , happyReduce_279),
	(280 , happyReduce_280),
	(281 , happyReduce_281),
	(282 , happyReduce_282),
	(283 , happyReduce_283),
	(284 , happyReduce_284),
	(285 , happyReduce_285),
	(286 , happyReduce_286),
	(287 , happyReduce_287),
	(288 , happyReduce_288),
	(289 , happyReduce_289),
	(290 , happyReduce_290),
	(291 , happyReduce_291),
	(292 , happyReduce_292),
	(293 , happyReduce_293),
	(294 , happyReduce_294),
	(295 , happyReduce_295),
	(296 , happyReduce_296),
	(297 , happyReduce_297),
	(298 , happyReduce_298),
	(299 , happyReduce_299),
	(300 , happyReduce_300),
	(301 , happyReduce_301),
	(302 , happyReduce_302)
	]

happy_n_terms = 66 :: Int
happy_n_nonterms = 133 :: Int

happyReduce_1 = happyReduce 6# 0# happyReduction_1
happyReduction_1 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut131 happy_x_3 of { happy_var_3 -> 
	case happyOut9 happy_x_4 of { happy_var_4 -> 
	case happyOut5 happy_x_6 of { happy_var_6 -> 
	happyIn4
		 (HsModule happy_var_1 happy_var_3 happy_var_4 (fst happy_var_6) (snd happy_var_6)
	) `HappyStk` happyRest}}}}

happyReduce_2 = happySpecReduce_2 0# happyReduction_2
happyReduction_2 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn4
		 (HsModule happy_var_1 main_mod (Just [HsEVar (UnQual (main_name happy_var_1 ))])
							(fst happy_var_2) (snd happy_var_2)
	)}}

happyReduce_3 = happySpecReduce_3 1# happyReduction_3
happyReduction_3 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2
	)}

happyReduce_4 = happySpecReduce_3 1# happyReduction_4
happyReduction_4 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut6 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (happy_var_2
	)}

happyReduce_5 = happyReduce 4# 2# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut14 happy_x_2 of { happy_var_2 -> 
	case happyOut29 happy_x_4 of { happy_var_4 -> 
	happyIn6
		 ((reverse happy_var_2, happy_var_4)
	) `HappyStk` happyRest}}

happyReduce_6 = happySpecReduce_2 2# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 (([], happy_var_2)
	)}

happyReduce_7 = happySpecReduce_3 2# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_2 of { happy_var_2 -> 
	happyIn6
		 ((reverse happy_var_2, [])
	)}

happyReduce_8 = happySpecReduce_1 2# happyReduction_8
happyReduction_8 happy_x_1
	 =  happyIn6
		 (([], [])
	)

happyReduce_9 = happySpecReduce_2 3# happyReduction_9
happyReduction_9 happy_x_2
	happy_x_1
	 =  happyIn7
		 (()
	)

happyReduce_10 = happySpecReduce_1 4# happyReduction_10
happyReduction_10 happy_x_1
	 =  happyIn8
		 (()
	)

happyReduce_11 = happySpecReduce_0 4# happyReduction_11
happyReduction_11  =  happyIn8
		 (()
	)

happyReduce_12 = happySpecReduce_1 5# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOut10 happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (Just happy_var_1
	)}

happyReduce_13 = happySpecReduce_0 5# happyReduction_13
happyReduction_13  =  happyIn9
		 (Nothing
	)

happyReduce_14 = happyReduce 4# 6# happyReduction_14
happyReduction_14 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut12 happy_x_2 of { happy_var_2 -> 
	happyIn10
		 (reverse happy_var_2
	) `HappyStk` happyRest}

happyReduce_15 = happySpecReduce_3 6# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn10
		 ([]
	)

happyReduce_16 = happySpecReduce_1 7# happyReduction_16
happyReduction_16 happy_x_1
	 =  happyIn11
		 (()
	)

happyReduce_17 = happySpecReduce_0 7# happyReduction_17
happyReduction_17  =  happyIn11
		 (()
	)

happyReduce_18 = happySpecReduce_3 8# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut12 happy_x_1 of { happy_var_1 -> 
	case happyOut13 happy_x_3 of { happy_var_3 -> 
	happyIn12
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_19 = happySpecReduce_1 8# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut13 happy_x_1 of { happy_var_1 -> 
	happyIn12
		 ([happy_var_1]
	)}

happyReduce_20 = happySpecReduce_1 9# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut104 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (HsEVar happy_var_1
	)}

happyReduce_21 = happySpecReduce_1 9# happyReduction_21
happyReduction_21 happy_x_1
	 =  case happyOut134 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (HsEAbs happy_var_1
	)}

happyReduce_22 = happyReduce 4# 9# happyReduction_22
happyReduction_22 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut134 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (HsEThingAll happy_var_1
	) `HappyStk` happyRest}

happyReduce_23 = happySpecReduce_3 9# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut134 happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (HsEThingWith happy_var_1 []
	)}

happyReduce_24 = happyReduce 4# 9# happyReduction_24
happyReduction_24 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut134 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn13
		 (HsEThingWith happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_25 = happySpecReduce_2 9# happyReduction_25
happyReduction_25 happy_x_2
	happy_x_1
	 =  case happyOut131 happy_x_2 of { happy_var_2 -> 
	happyIn13
		 (HsEModuleContents happy_var_2
	)}

happyReduce_26 = happySpecReduce_3 10# happyReduction_26
happyReduction_26 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut14 happy_x_1 of { happy_var_1 -> 
	case happyOut15 happy_x_3 of { happy_var_3 -> 
	happyIn14
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_27 = happySpecReduce_1 10# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut15 happy_x_1 of { happy_var_1 -> 
	happyIn14
		 ([happy_var_1]
	)}

happyReduce_28 = happyReduce 6# 11# happyReduction_28
happyReduction_28 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut16 happy_x_3 of { happy_var_3 -> 
	case happyOut131 happy_x_4 of { happy_var_4 -> 
	case happyOut17 happy_x_5 of { happy_var_5 -> 
	case happyOut18 happy_x_6 of { happy_var_6 -> 
	happyIn15
		 (HsImportDecl happy_var_1 happy_var_4 happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}}}

happyReduce_29 = happySpecReduce_1 12# happyReduction_29
happyReduction_29 happy_x_1
	 =  happyIn16
		 (True
	)

happyReduce_30 = happySpecReduce_0 12# happyReduction_30
happyReduction_30  =  happyIn16
		 (False
	)

happyReduce_31 = happySpecReduce_2 13# happyReduction_31
happyReduction_31 happy_x_2
	happy_x_1
	 =  case happyOut131 happy_x_2 of { happy_var_2 -> 
	happyIn17
		 (Just happy_var_2
	)}

happyReduce_32 = happySpecReduce_0 13# happyReduction_32
happyReduction_32  =  happyIn17
		 (Nothing
	)

happyReduce_33 = happySpecReduce_1 14# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut19 happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (Just happy_var_1
	)}

happyReduce_34 = happySpecReduce_0 14# happyReduction_34
happyReduction_34  =  happyIn18
		 (Nothing
	)

happyReduce_35 = happyReduce 5# 15# happyReduction_35
happyReduction_35 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_1 of { happy_var_1 -> 
	case happyOut21 happy_x_3 of { happy_var_3 -> 
	happyIn19
		 ((happy_var_1, reverse happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_36 = happyReduce 4# 15# happyReduction_36
happyReduction_36 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut20 happy_x_1 of { happy_var_1 -> 
	happyIn19
		 ((happy_var_1, [])
	) `HappyStk` happyRest}

happyReduce_37 = happySpecReduce_1 16# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn20
		 (True
	)

happyReduce_38 = happySpecReduce_0 16# happyReduction_38
happyReduction_38  =  happyIn20
		 (False
	)

happyReduce_39 = happySpecReduce_3 17# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut21 happy_x_1 of { happy_var_1 -> 
	case happyOut22 happy_x_3 of { happy_var_3 -> 
	happyIn21
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_40 = happySpecReduce_1 17# happyReduction_40
happyReduction_40 happy_x_1
	 =  case happyOut22 happy_x_1 of { happy_var_1 -> 
	happyIn21
		 ([happy_var_1]
	)}

happyReduce_41 = happySpecReduce_1 18# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut103 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (HsIVar happy_var_1
	)}

happyReduce_42 = happySpecReduce_1 18# happyReduction_42
happyReduction_42 happy_x_1
	 =  case happyOut132 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (HsIAbs happy_var_1
	)}

happyReduce_43 = happyReduce 4# 18# happyReduction_43
happyReduction_43 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut132 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (HsIThingAll happy_var_1
	) `HappyStk` happyRest}

happyReduce_44 = happySpecReduce_3 18# happyReduction_44
happyReduction_44 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut132 happy_x_1 of { happy_var_1 -> 
	happyIn22
		 (HsIThingWith happy_var_1 []
	)}

happyReduce_45 = happyReduce 4# 18# happyReduction_45
happyReduction_45 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut132 happy_x_1 of { happy_var_1 -> 
	case happyOut23 happy_x_3 of { happy_var_3 -> 
	happyIn22
		 (HsIThingWith happy_var_1 (reverse happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_46 = happySpecReduce_3 19# happyReduction_46
happyReduction_46 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { happy_var_1 -> 
	case happyOut24 happy_x_3 of { happy_var_3 -> 
	happyIn23
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_47 = happySpecReduce_1 19# happyReduction_47
happyReduction_47 happy_x_1
	 =  case happyOut24 happy_x_1 of { happy_var_1 -> 
	happyIn23
		 ([happy_var_1]
	)}

happyReduce_48 = happySpecReduce_1 20# happyReduction_48
happyReduction_48 happy_x_1
	 =  case happyOut103 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (HsVarName happy_var_1
	)}

happyReduce_49 = happySpecReduce_1 20# happyReduction_49
happyReduction_49 happy_x_1
	 =  case happyOut105 happy_x_1 of { happy_var_1 -> 
	happyIn24
		 (HsConName happy_var_1
	)}

happyReduce_50 = happyReduce 4# 21# happyReduction_50
happyReduction_50 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut27 happy_x_2 of { happy_var_2 -> 
	case happyOut26 happy_x_3 of { happy_var_3 -> 
	case happyOut28 happy_x_4 of { happy_var_4 -> 
	happyIn25
		 (HsInfixDecl happy_var_1 happy_var_2 happy_var_3 (reverse happy_var_4)
	) `HappyStk` happyRest}}}}

happyReduce_51 = happySpecReduce_0 22# happyReduction_51
happyReduction_51  =  happyIn26
		 (9
	)

happyReduce_52 = happyMonadReduce 1# 22# happyReduction_52
happyReduction_52 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOutTok happy_x_1 of { (IntTok happy_var_1) -> 
	 checkPrec happy_var_1}
	) (\r -> happyReturn (happyIn26 r))

happyReduce_53 = happySpecReduce_1 23# happyReduction_53
happyReduction_53 happy_x_1
	 =  happyIn27
		 (HsAssocNone
	)

happyReduce_54 = happySpecReduce_1 23# happyReduction_54
happyReduction_54 happy_x_1
	 =  happyIn27
		 (HsAssocLeft
	)

happyReduce_55 = happySpecReduce_1 23# happyReduction_55
happyReduction_55 happy_x_1
	 =  happyIn27
		 (HsAssocRight
	)

happyReduce_56 = happySpecReduce_3 24# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_1 of { happy_var_1 -> 
	case happyOut112 happy_x_3 of { happy_var_3 -> 
	happyIn28
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_57 = happySpecReduce_1 24# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut112 happy_x_1 of { happy_var_1 -> 
	happyIn28
		 ([happy_var_1]
	)}

happyReduce_58 = happyMonadReduce 2# 25# happyReduction_58
happyReduction_58 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut30 happy_x_1 of { happy_var_1 -> 
	 checkRevDecls happy_var_1}
	) (\r -> happyReturn (happyIn29 r))

happyReduce_59 = happySpecReduce_3 26# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut30 happy_x_1 of { happy_var_1 -> 
	case happyOut31 happy_x_3 of { happy_var_3 -> 
	happyIn30
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_60 = happySpecReduce_1 26# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut31 happy_x_1 of { happy_var_1 -> 
	happyIn30
		 ([happy_var_1]
	)}

happyReduce_61 = happyReduce 5# 27# happyReduction_61
happyReduction_61 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut50 happy_x_3 of { happy_var_3 -> 
	case happyOut43 happy_x_5 of { happy_var_5 -> 
	happyIn31
		 (HsTypeDecl happy_var_1 (fst happy_var_3) (snd happy_var_3) happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_62 = happyMonadReduce 6# 27# happyReduction_62
happyReduction_62 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	case happyOut52 happy_x_5 of { happy_var_5 -> 
	case happyOut61 happy_x_6 of { happy_var_6 -> 
	 do { (cs,c,t) <- checkDataHeader happy_var_3;
				return (HsDataDecl happy_var_1 cs c t (reverse happy_var_5) happy_var_6) }}}}}
	) (\r -> happyReturn (happyIn31 r))

happyReduce_63 = happyMonadReduce 6# 27# happyReduction_63
happyReduction_63 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	case happyOut53 happy_x_5 of { happy_var_5 -> 
	case happyOut61 happy_x_6 of { happy_var_6 -> 
	 do { (cs,c,t) <- checkDataHeader happy_var_3;
				return (HsNewTypeDecl happy_var_1 cs c t happy_var_5 happy_var_6) }}}}}
	) (\r -> happyReturn (happyIn31 r))

happyReduce_64 = happyMonadReduce 4# 27# happyReduction_64
happyReduction_64 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	case happyOut63 happy_x_4 of { happy_var_4 -> 
	 do { (cs,c,vs) <- checkClassHeader happy_var_3;
				return (HsClassDecl happy_var_1 cs c vs happy_var_4) }}}}
	) (\r -> happyReturn (happyIn31 r))

happyReduce_65 = happyMonadReduce 4# 27# happyReduction_65
happyReduction_65 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut47 happy_x_3 of { happy_var_3 -> 
	case happyOut64 happy_x_4 of { happy_var_4 -> 
	 do { (cs,c,ts) <- checkInstHeader happy_var_3;
				return (HsInstDecl happy_var_1 cs c ts happy_var_4) }}}}
	) (\r -> happyReturn (happyIn31 r))

happyReduce_66 = happyReduce 5# 27# happyReduction_66
happyReduction_66 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut32 happy_x_4 of { happy_var_4 -> 
	happyIn31
		 (HsDefaultDecl happy_var_1 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_67 = happySpecReduce_1 27# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut39 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_1 27# happyReduction_68
happyReduction_68 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn31
		 (happy_var_1
	)}

happyReduce_69 = happySpecReduce_1 28# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 (reverse happy_var_1
	)}

happyReduce_70 = happySpecReduce_1 28# happyReduction_70
happyReduction_70 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn32
		 ([happy_var_1]
	)}

happyReduce_71 = happySpecReduce_0 28# happyReduction_71
happyReduction_71  =  happyIn32
		 ([]
	)

happyReduce_72 = happyMonadReduce 3# 29# happyReduction_72
happyReduction_72 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut34 happy_x_2 of { happy_var_2 -> 
	 checkRevDecls happy_var_2}
	) (\r -> happyReturn (happyIn33 r))

happyReduce_73 = happySpecReduce_1 29# happyReduction_73
happyReduction_73 happy_x_1
	 =  happyIn33
		 ([]
	)

happyReduce_74 = happySpecReduce_3 30# happyReduction_74
happyReduction_74 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { happy_var_1 -> 
	case happyOut35 happy_x_3 of { happy_var_3 -> 
	happyIn34
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_75 = happySpecReduce_1 30# happyReduction_75
happyReduction_75 happy_x_1
	 =  case happyOut35 happy_x_1 of { happy_var_1 -> 
	happyIn34
		 ([happy_var_1]
	)}

happyReduce_76 = happySpecReduce_1 31# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut37 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_77 = happySpecReduce_1 31# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut25 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_78 = happySpecReduce_1 31# happyReduction_78
happyReduction_78 happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	happyIn35
		 (happy_var_1
	)}

happyReduce_79 = happySpecReduce_3 32# happyReduction_79
happyReduction_79 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (happy_var_2
	)}

happyReduce_80 = happySpecReduce_3 32# happyReduction_80
happyReduction_80 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_2 of { happy_var_2 -> 
	happyIn36
		 (happy_var_2
	)}

happyReduce_81 = happyReduce 4# 33# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut38 happy_x_2 of { happy_var_2 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	happyIn37
		 (HsTypeSig happy_var_1 (reverse happy_var_2) happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_82 = happySpecReduce_3 34# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut103 happy_x_3 of { happy_var_3 -> 
	happyIn38
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_83 = happyMonadReduce 1# 34# happyReduction_83
happyReduction_83 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut104 happy_x_1 of { happy_var_1 -> 
	 do { n <- checkUnQual happy_var_1;
						return [n] }}
	) (\r -> happyReturn (happyIn38 r))

happyReduce_84 = happyReduce 9# 35# happyReduction_84
happyReduction_84 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { (VarId happy_var_4) -> 
	case happyOut40 happy_x_5 of { happy_var_5 -> 
	case happyOut41 happy_x_6 of { happy_var_6 -> 
	case happyOut42 happy_x_7 of { happy_var_7 -> 
	case happyOut43 happy_x_9 of { happy_var_9 -> 
	happyIn39
		 (HsForeignImport happy_var_1 happy_var_4 happy_var_5 happy_var_6 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}}}

happyReduce_85 = happyReduce 8# 35# happyReduction_85
happyReduction_85 (happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_4 of { (VarId happy_var_4) -> 
	case happyOut41 happy_x_5 of { happy_var_5 -> 
	case happyOut42 happy_x_6 of { happy_var_6 -> 
	case happyOut43 happy_x_8 of { happy_var_8 -> 
	happyIn39
		 (HsForeignExport happy_var_1 happy_var_4 happy_var_5 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}}

happyReduce_86 = happySpecReduce_1 36# happyReduction_86
happyReduction_86 happy_x_1
	 =  happyIn40
		 (HsSafe
	)

happyReduce_87 = happySpecReduce_1 36# happyReduction_87
happyReduction_87 happy_x_1
	 =  happyIn40
		 (HsUnsafe
	)

happyReduce_88 = happySpecReduce_0 36# happyReduction_88
happyReduction_88  =  happyIn40
		 (HsSafe
	)

happyReduce_89 = happySpecReduce_1 37# happyReduction_89
happyReduction_89 happy_x_1
	 =  case happyOutTok happy_x_1 of { (StringTok happy_var_1) -> 
	happyIn41
		 (happy_var_1
	)}

happyReduce_90 = happySpecReduce_0 37# happyReduction_90
happyReduction_90  =  happyIn41
		 (""
	)

happyReduce_91 = happySpecReduce_2 38# happyReduction_91
happyReduction_91 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (VarId happy_var_2) -> 
	happyIn42
		 (HsIdent happy_var_1 happy_var_2
	)}}

happyReduce_92 = happySpecReduce_3 38# happyReduction_92
happyReduction_92 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut124 happy_x_2 of { happy_var_2 -> 
	happyIn42
		 (happy_var_2
	)}

happyReduce_93 = happySpecReduce_3 39# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn43
		 (HsTyFun happy_var_1 happy_var_3
	)}}

happyReduce_94 = happySpecReduce_1 39# happyReduction_94
happyReduction_94 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn43
		 (happy_var_1
	)}

happyReduce_95 = happySpecReduce_2 40# happyReduction_95
happyReduction_95 happy_x_2
	happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn44
		 (HsTyApp happy_var_1 happy_var_2
	)}}

happyReduce_96 = happySpecReduce_1 40# happyReduction_96
happyReduction_96 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn44
		 (happy_var_1
	)}

happyReduce_97 = happySpecReduce_1 41# happyReduction_97
happyReduction_97 happy_x_1
	 =  case happyOut46 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (HsTyCon happy_var_1
	)}

happyReduce_98 = happySpecReduce_1 41# happyReduction_98
happyReduction_98 happy_x_1
	 =  case happyOut136 happy_x_1 of { happy_var_1 -> 
	happyIn45
		 (HsTyVar happy_var_1
	)}

happyReduce_99 = happySpecReduce_3 41# happyReduction_99
happyReduction_99 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (HsTyTuple (reverse happy_var_2)
	)}

happyReduce_100 = happySpecReduce_3 41# happyReduction_100
happyReduction_100 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (HsTyApp list_tycon happy_var_2
	)}

happyReduce_101 = happySpecReduce_3 41# happyReduction_101
happyReduction_101 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { happy_var_2 -> 
	happyIn45
		 (happy_var_2
	)}

happyReduce_102 = happySpecReduce_1 42# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut118 happy_x_1 of { happy_var_1 -> 
	happyIn46
		 (happy_var_1
	)}

happyReduce_103 = happySpecReduce_2 42# happyReduction_103
happyReduction_103 happy_x_2
	happy_x_1
	 =  happyIn46
		 (unit_tycon_name
	)

happyReduce_104 = happySpecReduce_3 42# happyReduction_104
happyReduction_104 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn46
		 (fun_tycon_name
	)

happyReduce_105 = happySpecReduce_2 42# happyReduction_105
happyReduction_105 happy_x_2
	happy_x_1
	 =  happyIn46
		 (list_tycon_name
	)

happyReduce_106 = happySpecReduce_3 42# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_2 of { happy_var_2 -> 
	happyIn46
		 (tuple_tycon_name happy_var_2
	)}

happyReduce_107 = happySpecReduce_3 43# happyReduction_107
happyReduction_107 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn47
		 (HsQualType happy_var_1 happy_var_3
	)}}

happyReduce_108 = happySpecReduce_1 43# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn47
		 (HsQualType [] happy_var_1
	)}

happyReduce_109 = happyMonadReduce 1# 44# happyReduction_109
happyReduction_109 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	 checkContext happy_var_1}
	) (\r -> happyReturn (happyIn48 r))

happyReduce_110 = happySpecReduce_3 45# happyReduction_110
happyReduction_110 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_111 = happySpecReduce_3 45# happyReduction_111
happyReduction_111 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	case happyOut43 happy_x_3 of { happy_var_3 -> 
	happyIn49
		 ([happy_var_3, happy_var_1]
	)}}

happyReduce_112 = happySpecReduce_2 46# happyReduction_112
happyReduction_112 happy_x_2
	happy_x_1
	 =  case happyOut133 happy_x_1 of { happy_var_1 -> 
	case happyOut51 happy_x_2 of { happy_var_2 -> 
	happyIn50
		 ((happy_var_1,reverse happy_var_2)
	)}}

happyReduce_113 = happySpecReduce_2 47# happyReduction_113
happyReduction_113 happy_x_2
	happy_x_1
	 =  case happyOut51 happy_x_1 of { happy_var_1 -> 
	case happyOut136 happy_x_2 of { happy_var_2 -> 
	happyIn51
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_114 = happySpecReduce_0 47# happyReduction_114
happyReduction_114  =  happyIn51
		 ([]
	)

happyReduce_115 = happySpecReduce_3 48# happyReduction_115
happyReduction_115 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut52 happy_x_1 of { happy_var_1 -> 
	case happyOut53 happy_x_3 of { happy_var_3 -> 
	happyIn52
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_116 = happySpecReduce_1 48# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut53 happy_x_1 of { happy_var_1 -> 
	happyIn52
		 ([happy_var_1]
	)}

happyReduce_117 = happySpecReduce_2 49# happyReduction_117
happyReduction_117 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut54 happy_x_2 of { happy_var_2 -> 
	happyIn53
		 (HsConDecl happy_var_1 (fst happy_var_2) (snd happy_var_2)
	)}}

happyReduce_118 = happyReduce 4# 49# happyReduction_118
happyReduction_118 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut57 happy_x_2 of { happy_var_2 -> 
	case happyOut110 happy_x_3 of { happy_var_3 -> 
	case happyOut57 happy_x_4 of { happy_var_4 -> 
	happyIn53
		 (HsConDecl happy_var_1 happy_var_3 [happy_var_2,happy_var_4]
	) `HappyStk` happyRest}}}}

happyReduce_119 = happyReduce 4# 49# happyReduction_119
happyReduction_119 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut105 happy_x_2 of { happy_var_2 -> 
	happyIn53
		 (HsRecDecl happy_var_1 happy_var_2 []
	) `HappyStk` happyRest}}

happyReduce_120 = happyReduce 5# 49# happyReduction_120
happyReduction_120 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut105 happy_x_2 of { happy_var_2 -> 
	case happyOut58 happy_x_4 of { happy_var_4 -> 
	happyIn53
		 (HsRecDecl happy_var_1 happy_var_2 (reverse happy_var_4)
	) `HappyStk` happyRest}}}

happyReduce_121 = happyMonadReduce 1# 50# happyReduction_121
happyReduction_121 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	 do { (c,ts) <- splitTyConApp happy_var_1;
						return (c,map HsUnBangedTy ts) }}
	) (\r -> happyReturn (happyIn54 r))

happyReduce_122 = happySpecReduce_1 50# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_123 = happyMonadReduce 3# 51# happyReduction_123
happyReduction_123 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut44 happy_x_1 of { happy_var_1 -> 
	case happyOut45 happy_x_3 of { happy_var_3 -> 
	 do { (c,ts) <- splitTyConApp happy_var_1;
						return (c,map HsUnBangedTy ts++
							[HsBangedTy happy_var_3]) }}}
	) (\r -> happyReturn (happyIn55 r))

happyReduce_124 = happySpecReduce_2 51# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { happy_var_1 -> 
	case happyOut56 happy_x_2 of { happy_var_2 -> 
	happyIn55
		 ((fst happy_var_1, snd happy_var_1 ++ [happy_var_2] )
	)}}

happyReduce_125 = happySpecReduce_1 52# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut45 happy_x_1 of { happy_var_1 -> 
	happyIn56
		 (HsUnBangedTy happy_var_1
	)}

happyReduce_126 = happySpecReduce_2 52# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn56
		 (HsBangedTy   happy_var_2
	)}

happyReduce_127 = happySpecReduce_1 53# happyReduction_127
happyReduction_127 happy_x_1
	 =  case happyOut44 happy_x_1 of { happy_var_1 -> 
	happyIn57
		 (HsUnBangedTy happy_var_1
	)}

happyReduce_128 = happySpecReduce_2 53# happyReduction_128
happyReduction_128 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn57
		 (HsBangedTy   happy_var_2
	)}

happyReduce_129 = happySpecReduce_3 54# happyReduction_129
happyReduction_129 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { happy_var_1 -> 
	case happyOut59 happy_x_3 of { happy_var_3 -> 
	happyIn58
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_130 = happySpecReduce_1 54# happyReduction_130
happyReduction_130 happy_x_1
	 =  case happyOut59 happy_x_1 of { happy_var_1 -> 
	happyIn58
		 ([happy_var_1]
	)}

happyReduce_131 = happySpecReduce_3 55# happyReduction_131
happyReduction_131 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut38 happy_x_1 of { happy_var_1 -> 
	case happyOut60 happy_x_3 of { happy_var_3 -> 
	happyIn59
		 ((reverse happy_var_1, happy_var_3)
	)}}

happyReduce_132 = happySpecReduce_1 56# happyReduction_132
happyReduction_132 happy_x_1
	 =  case happyOut43 happy_x_1 of { happy_var_1 -> 
	happyIn60
		 (HsUnBangedTy happy_var_1
	)}

happyReduce_133 = happySpecReduce_2 56# happyReduction_133
happyReduction_133 happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_2 of { happy_var_2 -> 
	happyIn60
		 (HsBangedTy   happy_var_2
	)}

happyReduce_134 = happySpecReduce_0 57# happyReduction_134
happyReduction_134  =  happyIn61
		 ([]
	)

happyReduce_135 = happySpecReduce_2 57# happyReduction_135
happyReduction_135 happy_x_2
	happy_x_1
	 =  case happyOut135 happy_x_2 of { happy_var_2 -> 
	happyIn61
		 ([happy_var_2]
	)}

happyReduce_136 = happySpecReduce_3 57# happyReduction_136
happyReduction_136 happy_x_3
	happy_x_2
	happy_x_1
	 =  happyIn61
		 ([]
	)

happyReduce_137 = happyReduce 4# 57# happyReduction_137
happyReduction_137 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut62 happy_x_3 of { happy_var_3 -> 
	happyIn61
		 (reverse happy_var_3
	) `HappyStk` happyRest}

happyReduce_138 = happySpecReduce_3 58# happyReduction_138
happyReduction_138 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut62 happy_x_1 of { happy_var_1 -> 
	case happyOut135 happy_x_3 of { happy_var_3 -> 
	happyIn62
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_139 = happySpecReduce_1 58# happyReduction_139
happyReduction_139 happy_x_1
	 =  case happyOut135 happy_x_1 of { happy_var_1 -> 
	happyIn62
		 ([happy_var_1]
	)}

happyReduce_140 = happyMonadReduce 2# 59# happyReduction_140
happyReduction_140 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut36 happy_x_2 of { happy_var_2 -> 
	 checkClassBody happy_var_2}
	) (\r -> happyReturn (happyIn63 r))

happyReduce_141 = happySpecReduce_0 59# happyReduction_141
happyReduction_141  =  happyIn63
		 ([]
	)

happyReduce_142 = happyMonadReduce 4# 60# happyReduction_142
happyReduction_142 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut65 happy_x_3 of { happy_var_3 -> 
	 checkClassBody happy_var_3}
	) (\r -> happyReturn (happyIn64 r))

happyReduce_143 = happyMonadReduce 4# 60# happyReduction_143
happyReduction_143 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut65 happy_x_3 of { happy_var_3 -> 
	 checkClassBody happy_var_3}
	) (\r -> happyReturn (happyIn64 r))

happyReduce_144 = happySpecReduce_0 60# happyReduction_144
happyReduction_144  =  happyIn64
		 ([]
	)

happyReduce_145 = happyMonadReduce 3# 61# happyReduction_145
happyReduction_145 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut66 happy_x_2 of { happy_var_2 -> 
	 checkRevDecls happy_var_2}
	) (\r -> happyReturn (happyIn65 r))

happyReduce_146 = happySpecReduce_1 61# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn65
		 ([]
	)

happyReduce_147 = happySpecReduce_3 62# happyReduction_147
happyReduction_147 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut66 happy_x_1 of { happy_var_1 -> 
	case happyOut67 happy_x_3 of { happy_var_3 -> 
	happyIn66
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_148 = happySpecReduce_1 62# happyReduction_148
happyReduction_148 happy_x_1
	 =  case happyOut67 happy_x_1 of { happy_var_1 -> 
	happyIn66
		 ([happy_var_1]
	)}

happyReduce_149 = happyMonadReduce 4# 63# happyReduction_149
happyReduction_149 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut75 happy_x_2 of { happy_var_2 -> 
	case happyOut69 happy_x_3 of { happy_var_3 -> 
	case happyOut68 happy_x_4 of { happy_var_4 -> 
	 checkValDef happy_var_1 happy_var_2 happy_var_3 happy_var_4}}}}
	) (\r -> happyReturn (happyIn67 r))

happyReduce_150 = happySpecReduce_2 64# happyReduction_150
happyReduction_150 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn68
		 (happy_var_2
	)}

happyReduce_151 = happySpecReduce_0 64# happyReduction_151
happyReduction_151  =  happyIn68
		 ([]
	)

happyReduce_152 = happyMonadReduce 2# 65# happyReduction_152
happyReduction_152 (happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut72 happy_x_2 of { happy_var_2 -> 
	 do { e <- checkExpr happy_var_2;
						return (HsUnGuardedRhs e) }}
	) (\r -> happyReturn (happyIn69 r))

happyReduce_153 = happySpecReduce_1 65# happyReduction_153
happyReduction_153 happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	happyIn69
		 (HsGuardedRhss  (reverse happy_var_1)
	)}

happyReduce_154 = happySpecReduce_2 66# happyReduction_154
happyReduction_154 happy_x_2
	happy_x_1
	 =  case happyOut70 happy_x_1 of { happy_var_1 -> 
	case happyOut71 happy_x_2 of { happy_var_2 -> 
	happyIn70
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_155 = happySpecReduce_1 66# happyReduction_155
happyReduction_155 happy_x_1
	 =  case happyOut71 happy_x_1 of { happy_var_1 -> 
	happyIn70
		 ([happy_var_1]
	)}

happyReduce_156 = happyMonadReduce 5# 67# happyReduction_156
happyReduction_156 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut73 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	 do { g <- checkExpr happy_var_3;
						e <- checkExpr happy_var_5;
						return (HsGuardedRhs happy_var_1 g e) }}}}
	) (\r -> happyReturn (happyIn71 r))

happyReduce_157 = happyReduce 4# 68# happyReduction_157
happyReduction_157 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut128 happy_x_3 of { happy_var_3 -> 
	case happyOut47 happy_x_4 of { happy_var_4 -> 
	happyIn72
		 (HsExpTypeSig happy_var_3 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_158 = happySpecReduce_1 68# happyReduction_158
happyReduction_158 happy_x_1
	 =  case happyOut73 happy_x_1 of { happy_var_1 -> 
	happyIn72
		 (happy_var_1
	)}

happyReduce_159 = happySpecReduce_1 69# happyReduction_159
happyReduction_159 happy_x_1
	 =  case happyOut74 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 (happy_var_1
	)}

happyReduce_160 = happySpecReduce_1 69# happyReduction_160
happyReduction_160 happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	happyIn73
		 (happy_var_1
	)}

happyReduce_161 = happySpecReduce_3 70# happyReduction_161
happyReduction_161 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut113 happy_x_2 of { happy_var_2 -> 
	case happyOut76 happy_x_3 of { happy_var_3 -> 
	happyIn74
		 (HsInfixApp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_162 = happySpecReduce_1 70# happyReduction_162
happyReduction_162 happy_x_1
	 =  case happyOut76 happy_x_1 of { happy_var_1 -> 
	happyIn74
		 (happy_var_1
	)}

happyReduce_163 = happySpecReduce_3 71# happyReduction_163
happyReduction_163 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut75 happy_x_1 of { happy_var_1 -> 
	case happyOut113 happy_x_2 of { happy_var_2 -> 
	case happyOut77 happy_x_3 of { happy_var_3 -> 
	happyIn75
		 (HsInfixApp happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_164 = happySpecReduce_1 71# happyReduction_164
happyReduction_164 happy_x_1
	 =  case happyOut77 happy_x_1 of { happy_var_1 -> 
	happyIn75
		 (happy_var_1
	)}

happyReduce_165 = happyReduce 5# 72# happyReduction_165
happyReduction_165 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_2 of { happy_var_2 -> 
	case happyOut79 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	happyIn76
		 (HsLambda happy_var_2 (reverse happy_var_3) happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_166 = happyReduce 4# 72# happyReduction_166
happyReduction_166 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	happyIn76
		 (HsLet happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_167 = happyReduce 6# 72# happyReduction_167
happyReduction_167 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	case happyOut72 happy_x_6 of { happy_var_6 -> 
	happyIn76
		 (HsIf happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_168 = happyReduce 4# 73# happyReduction_168
happyReduction_168 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut72 happy_x_2 of { happy_var_2 -> 
	case happyOut90 happy_x_4 of { happy_var_4 -> 
	happyIn77
		 (HsCase happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_169 = happySpecReduce_2 73# happyReduction_169
happyReduction_169 happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (HsNegApp happy_var_2
	)}

happyReduce_170 = happySpecReduce_2 73# happyReduction_170
happyReduction_170 happy_x_2
	happy_x_1
	 =  case happyOut98 happy_x_2 of { happy_var_2 -> 
	happyIn77
		 (HsDo happy_var_2
	)}

happyReduce_171 = happySpecReduce_1 73# happyReduction_171
happyReduction_171 happy_x_1
	 =  case happyOut78 happy_x_1 of { happy_var_1 -> 
	happyIn77
		 (happy_var_1
	)}

happyReduce_172 = happySpecReduce_2 74# happyReduction_172
happyReduction_172 happy_x_2
	happy_x_1
	 =  case happyOut78 happy_x_1 of { happy_var_1 -> 
	case happyOut81 happy_x_2 of { happy_var_2 -> 
	happyIn78
		 (HsApp happy_var_1 happy_var_2
	)}}

happyReduce_173 = happySpecReduce_1 74# happyReduction_173
happyReduction_173 happy_x_1
	 =  case happyOut81 happy_x_1 of { happy_var_1 -> 
	happyIn78
		 (happy_var_1
	)}

happyReduce_174 = happySpecReduce_2 75# happyReduction_174
happyReduction_174 happy_x_2
	happy_x_1
	 =  case happyOut79 happy_x_1 of { happy_var_1 -> 
	case happyOut80 happy_x_2 of { happy_var_2 -> 
	happyIn79
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_175 = happySpecReduce_1 75# happyReduction_175
happyReduction_175 happy_x_1
	 =  case happyOut80 happy_x_1 of { happy_var_1 -> 
	happyIn79
		 ([happy_var_1]
	)}

happyReduce_176 = happyMonadReduce 1# 76# happyReduction_176
happyReduction_176 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut81 happy_x_1 of { happy_var_1 -> 
	 checkPattern happy_var_1}
	) (\r -> happyReturn (happyIn80 r))

happyReduce_177 = happyMonadReduce 3# 77# happyReduction_177
happyReduction_177 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut104 happy_x_1 of { happy_var_1 -> 
	case happyOut81 happy_x_3 of { happy_var_3 -> 
	 do { n <- checkUnQual happy_var_1;
						return (HsAsPat n happy_var_3) }}}
	) (\r -> happyReturn (happyIn81 r))

happyReduce_178 = happySpecReduce_2 77# happyReduction_178
happyReduction_178 happy_x_2
	happy_x_1
	 =  case happyOut81 happy_x_2 of { happy_var_2 -> 
	happyIn81
		 (HsIrrPat happy_var_2
	)}

happyReduce_179 = happySpecReduce_1 77# happyReduction_179
happyReduction_179 happy_x_1
	 =  case happyOut82 happy_x_1 of { happy_var_1 -> 
	happyIn81
		 (happy_var_1
	)}

happyReduce_180 = happyMonadReduce 3# 78# happyReduction_180
happyReduction_180 (happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut82 happy_x_1 of { happy_var_1 -> 
	 mkRecConstrOrUpdate happy_var_1 []}
	) (\r -> happyReturn (happyIn82 r))

happyReduce_181 = happyMonadReduce 4# 78# happyReduction_181
happyReduction_181 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut82 happy_x_1 of { happy_var_1 -> 
	case happyOut100 happy_x_3 of { happy_var_3 -> 
	 mkRecConstrOrUpdate happy_var_1 (reverse happy_var_3)}}
	) (\r -> happyReturn (happyIn82 r))

happyReduce_182 = happySpecReduce_1 78# happyReduction_182
happyReduction_182 happy_x_1
	 =  case happyOut83 happy_x_1 of { happy_var_1 -> 
	happyIn82
		 (happy_var_1
	)}

happyReduce_183 = happySpecReduce_1 79# happyReduction_183
happyReduction_183 happy_x_1
	 =  case happyOut104 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 (HsVar happy_var_1
	)}

happyReduce_184 = happySpecReduce_1 79# happyReduction_184
happyReduction_184 happy_x_1
	 =  case happyOut102 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 (happy_var_1
	)}

happyReduce_185 = happySpecReduce_1 79# happyReduction_185
happyReduction_185 happy_x_1
	 =  case happyOut127 happy_x_1 of { happy_var_1 -> 
	happyIn83
		 (HsLit happy_var_1
	)}

happyReduce_186 = happySpecReduce_3 79# happyReduction_186
happyReduction_186 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_2 of { happy_var_2 -> 
	happyIn83
		 (HsParen happy_var_2
	)}

happyReduce_187 = happySpecReduce_3 79# happyReduction_187
happyReduction_187 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut85 happy_x_2 of { happy_var_2 -> 
	happyIn83
		 (HsTuple (reverse happy_var_2)
	)}

happyReduce_188 = happySpecReduce_3 79# happyReduction_188
happyReduction_188 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut86 happy_x_2 of { happy_var_2 -> 
	happyIn83
		 (happy_var_2
	)}

happyReduce_189 = happyReduce 4# 79# happyReduction_189
happyReduction_189 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut75 happy_x_2 of { happy_var_2 -> 
	case happyOut113 happy_x_3 of { happy_var_3 -> 
	happyIn83
		 (HsLeftSection happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_190 = happyReduce 4# 79# happyReduction_190
happyReduction_190 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut114 happy_x_2 of { happy_var_2 -> 
	case happyOut73 happy_x_3 of { happy_var_3 -> 
	happyIn83
		 (HsRightSection happy_var_2 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_191 = happySpecReduce_1 79# happyReduction_191
happyReduction_191 happy_x_1
	 =  happyIn83
		 (HsWildCard
	)

happyReduce_192 = happySpecReduce_2 80# happyReduction_192
happyReduction_192 happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_1 of { happy_var_1 -> 
	happyIn84
		 (happy_var_1 + 1
	)}

happyReduce_193 = happySpecReduce_1 80# happyReduction_193
happyReduction_193 happy_x_1
	 =  happyIn84
		 (1
	)

happyReduce_194 = happySpecReduce_3 81# happyReduction_194
happyReduction_194 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut85 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn85
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_195 = happySpecReduce_3 81# happyReduction_195
happyReduction_195 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn85
		 ([happy_var_3,happy_var_1]
	)}}

happyReduce_196 = happySpecReduce_1 82# happyReduction_196
happyReduction_196 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn86
		 (HsList [happy_var_1]
	)}

happyReduce_197 = happySpecReduce_1 82# happyReduction_197
happyReduction_197 happy_x_1
	 =  case happyOut87 happy_x_1 of { happy_var_1 -> 
	happyIn86
		 (HsList (reverse happy_var_1)
	)}

happyReduce_198 = happySpecReduce_2 82# happyReduction_198
happyReduction_198 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn86
		 (HsEnumFrom happy_var_1
	)}

happyReduce_199 = happyReduce 4# 82# happyReduction_199
happyReduction_199 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn86
		 (HsEnumFromThen happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_200 = happySpecReduce_3 82# happyReduction_200
happyReduction_200 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn86
		 (HsEnumFromTo happy_var_1 happy_var_3
	)}}

happyReduce_201 = happyReduce 5# 82# happyReduction_201
happyReduction_201 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	happyIn86
		 (HsEnumFromThenTo happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_202 = happySpecReduce_3 82# happyReduction_202
happyReduction_202 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut88 happy_x_3 of { happy_var_3 -> 
	happyIn86
		 (HsListComp happy_var_1 (reverse happy_var_3)
	)}}

happyReduce_203 = happySpecReduce_3 83# happyReduction_203
happyReduction_203 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut87 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn87
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_204 = happySpecReduce_3 83# happyReduction_204
happyReduction_204 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn87
		 ([happy_var_3,happy_var_1]
	)}}

happyReduce_205 = happySpecReduce_3 84# happyReduction_205
happyReduction_205 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut88 happy_x_1 of { happy_var_1 -> 
	case happyOut89 happy_x_3 of { happy_var_3 -> 
	happyIn88
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_206 = happySpecReduce_1 84# happyReduction_206
happyReduction_206 happy_x_1
	 =  case happyOut89 happy_x_1 of { happy_var_1 -> 
	happyIn88
		 ([happy_var_1]
	)}

happyReduce_207 = happyReduce 4# 85# happyReduction_207
happyReduction_207 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut97 happy_x_1 of { happy_var_1 -> 
	case happyOut128 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	happyIn89
		 (HsGenerator happy_var_2 happy_var_1 happy_var_4
	) `HappyStk` happyRest}}}

happyReduce_208 = happySpecReduce_1 85# happyReduction_208
happyReduction_208 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn89
		 (HsQualifier happy_var_1
	)}

happyReduce_209 = happySpecReduce_2 85# happyReduction_209
happyReduction_209 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { happy_var_2 -> 
	happyIn89
		 (HsLetStmt happy_var_2
	)}

happyReduce_210 = happySpecReduce_3 86# happyReduction_210
happyReduction_210 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut91 happy_x_2 of { happy_var_2 -> 
	happyIn90
		 (happy_var_2
	)}

happyReduce_211 = happySpecReduce_3 86# happyReduction_211
happyReduction_211 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut91 happy_x_2 of { happy_var_2 -> 
	happyIn90
		 (happy_var_2
	)}

happyReduce_212 = happySpecReduce_3 87# happyReduction_212
happyReduction_212 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut92 happy_x_2 of { happy_var_2 -> 
	happyIn91
		 (reverse happy_var_2
	)}

happyReduce_213 = happySpecReduce_3 88# happyReduction_213
happyReduction_213 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut92 happy_x_1 of { happy_var_1 -> 
	case happyOut93 happy_x_3 of { happy_var_3 -> 
	happyIn92
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_214 = happySpecReduce_1 88# happyReduction_214
happyReduction_214 happy_x_1
	 =  case happyOut93 happy_x_1 of { happy_var_1 -> 
	happyIn92
		 ([happy_var_1]
	)}

happyReduce_215 = happyReduce 4# 89# happyReduction_215
happyReduction_215 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut97 happy_x_2 of { happy_var_2 -> 
	case happyOut94 happy_x_3 of { happy_var_3 -> 
	case happyOut68 happy_x_4 of { happy_var_4 -> 
	happyIn93
		 (HsAlt happy_var_1 happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest}}}}

happyReduce_216 = happySpecReduce_2 90# happyReduction_216
happyReduction_216 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_2 of { happy_var_2 -> 
	happyIn94
		 (HsUnGuardedAlt happy_var_2
	)}

happyReduce_217 = happySpecReduce_1 90# happyReduction_217
happyReduction_217 happy_x_1
	 =  case happyOut95 happy_x_1 of { happy_var_1 -> 
	happyIn94
		 (HsGuardedAlts (reverse happy_var_1)
	)}

happyReduce_218 = happySpecReduce_2 91# happyReduction_218
happyReduction_218 happy_x_2
	happy_x_1
	 =  case happyOut95 happy_x_1 of { happy_var_1 -> 
	case happyOut96 happy_x_2 of { happy_var_2 -> 
	happyIn95
		 (happy_var_2 : happy_var_1
	)}}

happyReduce_219 = happySpecReduce_1 91# happyReduction_219
happyReduction_219 happy_x_1
	 =  case happyOut96 happy_x_1 of { happy_var_1 -> 
	happyIn95
		 ([happy_var_1]
	)}

happyReduce_220 = happyReduce 5# 92# happyReduction_220
happyReduction_220 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOut73 happy_x_3 of { happy_var_3 -> 
	case happyOut72 happy_x_5 of { happy_var_5 -> 
	happyIn96
		 (HsGuardedAlt happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_221 = happyMonadReduce 1# 93# happyReduction_221
happyReduction_221 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen (case happyOut75 happy_x_1 of { happy_var_1 -> 
	 checkPattern happy_var_1}
	) (\r -> happyReturn (happyIn97 r))

happyReduce_222 = happySpecReduce_3 94# happyReduction_222
happyReduction_222 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut99 happy_x_2 of { happy_var_2 -> 
	happyIn98
		 (happy_var_2
	)}

happyReduce_223 = happySpecReduce_3 94# happyReduction_223
happyReduction_223 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut99 happy_x_2 of { happy_var_2 -> 
	happyIn98
		 (happy_var_2
	)}

happyReduce_224 = happyReduce 4# 95# happyReduction_224
happyReduction_224 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { happy_var_2 -> 
	case happyOut99 happy_x_4 of { happy_var_4 -> 
	happyIn99
		 (HsLetStmt happy_var_2 : happy_var_4
	) `HappyStk` happyRest}}

happyReduce_225 = happyReduce 6# 95# happyReduction_225
happyReduction_225 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut97 happy_x_1 of { happy_var_1 -> 
	case happyOut128 happy_x_2 of { happy_var_2 -> 
	case happyOut72 happy_x_4 of { happy_var_4 -> 
	case happyOut99 happy_x_6 of { happy_var_6 -> 
	happyIn99
		 (HsGenerator happy_var_2 happy_var_1 happy_var_4 : happy_var_6
	) `HappyStk` happyRest}}}}

happyReduce_226 = happySpecReduce_3 95# happyReduction_226
happyReduction_226 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	case happyOut99 happy_x_3 of { happy_var_3 -> 
	happyIn99
		 (HsQualifier happy_var_1 : happy_var_3
	)}}

happyReduce_227 = happySpecReduce_2 95# happyReduction_227
happyReduction_227 happy_x_2
	happy_x_1
	 =  case happyOut99 happy_x_2 of { happy_var_2 -> 
	happyIn99
		 (happy_var_2
	)}

happyReduce_228 = happySpecReduce_2 95# happyReduction_228
happyReduction_228 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn99
		 ([HsQualifier happy_var_1]
	)}

happyReduce_229 = happySpecReduce_1 95# happyReduction_229
happyReduction_229 happy_x_1
	 =  case happyOut72 happy_x_1 of { happy_var_1 -> 
	happyIn99
		 ([HsQualifier happy_var_1]
	)}

happyReduce_230 = happySpecReduce_3 96# happyReduction_230
happyReduction_230 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut100 happy_x_1 of { happy_var_1 -> 
	case happyOut101 happy_x_3 of { happy_var_3 -> 
	happyIn100
		 (happy_var_3 : happy_var_1
	)}}

happyReduce_231 = happySpecReduce_1 96# happyReduction_231
happyReduction_231 happy_x_1
	 =  case happyOut101 happy_x_1 of { happy_var_1 -> 
	happyIn100
		 ([happy_var_1]
	)}

happyReduce_232 = happySpecReduce_3 97# happyReduction_232
happyReduction_232 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut104 happy_x_1 of { happy_var_1 -> 
	case happyOut72 happy_x_3 of { happy_var_3 -> 
	happyIn101
		 (HsFieldUpdate happy_var_1 happy_var_3
	)}}

happyReduce_233 = happySpecReduce_2 98# happyReduction_233
happyReduction_233 happy_x_2
	happy_x_1
	 =  happyIn102
		 (unit_con
	)

happyReduce_234 = happySpecReduce_2 98# happyReduction_234
happyReduction_234 happy_x_2
	happy_x_1
	 =  happyIn102
		 (HsList []
	)

happyReduce_235 = happySpecReduce_3 98# happyReduction_235
happyReduction_235 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut84 happy_x_2 of { happy_var_2 -> 
	happyIn102
		 (tuple_con happy_var_2
	)}

happyReduce_236 = happySpecReduce_1 98# happyReduction_236
happyReduction_236 happy_x_1
	 =  case happyOut106 happy_x_1 of { happy_var_1 -> 
	happyIn102
		 (HsCon happy_var_1
	)}

happyReduce_237 = happySpecReduce_1 99# happyReduction_237
happyReduction_237 happy_x_1
	 =  case happyOut117 happy_x_1 of { happy_var_1 -> 
	happyIn103
		 (happy_var_1
	)}

happyReduce_238 = happySpecReduce_3 99# happyReduction_238
happyReduction_238 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut124 happy_x_2 of { happy_var_2 -> 
	happyIn103
		 (happy_var_2
	)}

happyReduce_239 = happySpecReduce_1 100# happyReduction_239
happyReduction_239 happy_x_1
	 =  case happyOut116 happy_x_1 of { happy_var_1 -> 
	happyIn104
		 (happy_var_1
	)}

happyReduce_240 = happySpecReduce_3 100# happyReduction_240
happyReduction_240 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut122 happy_x_2 of { happy_var_2 -> 
	happyIn104
		 (happy_var_2
	)}

happyReduce_241 = happySpecReduce_1 101# happyReduction_241
happyReduction_241 happy_x_1
	 =  case happyOut119 happy_x_1 of { happy_var_1 -> 
	happyIn105
		 (happy_var_1
	)}

happyReduce_242 = happySpecReduce_3 101# happyReduction_242
happyReduction_242 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut121 happy_x_2 of { happy_var_2 -> 
	happyIn105
		 (happy_var_2
	)}

happyReduce_243 = happySpecReduce_1 102# happyReduction_243
happyReduction_243 happy_x_1
	 =  case happyOut118 happy_x_1 of { happy_var_1 -> 
	happyIn106
		 (happy_var_1
	)}

happyReduce_244 = happySpecReduce_3 102# happyReduction_244
happyReduction_244 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut115 happy_x_2 of { happy_var_2 -> 
	happyIn106
		 (happy_var_2
	)}

happyReduce_245 = happySpecReduce_1 103# happyReduction_245
happyReduction_245 happy_x_1
	 =  case happyOut124 happy_x_1 of { happy_var_1 -> 
	happyIn107
		 (happy_var_1
	)}

happyReduce_246 = happySpecReduce_3 103# happyReduction_246
happyReduction_246 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut117 happy_x_2 of { happy_var_2 -> 
	happyIn107
		 (happy_var_2
	)}

happyReduce_247 = happySpecReduce_1 104# happyReduction_247
happyReduction_247 happy_x_1
	 =  case happyOut122 happy_x_1 of { happy_var_1 -> 
	happyIn108
		 (happy_var_1
	)}

happyReduce_248 = happySpecReduce_3 104# happyReduction_248
happyReduction_248 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut116 happy_x_2 of { happy_var_2 -> 
	happyIn108
		 (happy_var_2
	)}

happyReduce_249 = happySpecReduce_1 105# happyReduction_249
happyReduction_249 happy_x_1
	 =  case happyOut123 happy_x_1 of { happy_var_1 -> 
	happyIn109
		 (happy_var_1
	)}

happyReduce_250 = happySpecReduce_3 105# happyReduction_250
happyReduction_250 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut116 happy_x_2 of { happy_var_2 -> 
	happyIn109
		 (happy_var_2
	)}

happyReduce_251 = happySpecReduce_1 106# happyReduction_251
happyReduction_251 happy_x_1
	 =  case happyOut121 happy_x_1 of { happy_var_1 -> 
	happyIn110
		 (happy_var_1
	)}

happyReduce_252 = happySpecReduce_3 106# happyReduction_252
happyReduction_252 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut119 happy_x_2 of { happy_var_2 -> 
	happyIn110
		 (happy_var_2
	)}

happyReduce_253 = happySpecReduce_1 107# happyReduction_253
happyReduction_253 happy_x_1
	 =  case happyOut115 happy_x_1 of { happy_var_1 -> 
	happyIn111
		 (happy_var_1
	)}

happyReduce_254 = happySpecReduce_3 107# happyReduction_254
happyReduction_254 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut118 happy_x_2 of { happy_var_2 -> 
	happyIn111
		 (happy_var_2
	)}

happyReduce_255 = happySpecReduce_1 108# happyReduction_255
happyReduction_255 happy_x_1
	 =  case happyOut107 happy_x_1 of { happy_var_1 -> 
	happyIn112
		 (HsVarOp happy_var_1
	)}

happyReduce_256 = happySpecReduce_1 108# happyReduction_256
happyReduction_256 happy_x_1
	 =  case happyOut110 happy_x_1 of { happy_var_1 -> 
	happyIn112
		 (HsConOp happy_var_1
	)}

happyReduce_257 = happySpecReduce_1 109# happyReduction_257
happyReduction_257 happy_x_1
	 =  case happyOut108 happy_x_1 of { happy_var_1 -> 
	happyIn113
		 (HsQVarOp happy_var_1
	)}

happyReduce_258 = happySpecReduce_1 109# happyReduction_258
happyReduction_258 happy_x_1
	 =  case happyOut111 happy_x_1 of { happy_var_1 -> 
	happyIn113
		 (HsQConOp happy_var_1
	)}

happyReduce_259 = happySpecReduce_1 110# happyReduction_259
happyReduction_259 happy_x_1
	 =  case happyOut109 happy_x_1 of { happy_var_1 -> 
	happyIn114
		 (HsQVarOp happy_var_1
	)}

happyReduce_260 = happySpecReduce_1 110# happyReduction_260
happyReduction_260 happy_x_1
	 =  case happyOut111 happy_x_1 of { happy_var_1 -> 
	happyIn114
		 (HsQConOp happy_var_1
	)}

happyReduce_261 = happySpecReduce_1 111# happyReduction_261
happyReduction_261 happy_x_1
	 =  happyIn115
		 (list_cons_name
	)

happyReduce_262 = happySpecReduce_1 111# happyReduction_262
happyReduction_262 happy_x_1
	 =  case happyOut120 happy_x_1 of { happy_var_1 -> 
	happyIn115
		 (happy_var_1
	)}

happyReduce_263 = happySpecReduce_1 112# happyReduction_263
happyReduction_263 happy_x_1
	 =  case happyOut117 happy_x_1 of { happy_var_1 -> 
	happyIn116
		 (UnQual happy_var_1
	)}

happyReduce_264 = happySpecReduce_2 112# happyReduction_264
happyReduction_264 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (QVarId happy_var_2) -> 
	happyIn116
		 (Qual (Module (fst happy_var_2)) (HsIdent happy_var_1 (snd happy_var_2))
	)}}

happyReduce_265 = happySpecReduce_2 113# happyReduction_265
happyReduction_265 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (VarId happy_var_2) -> 
	happyIn117
		 (HsIdent happy_var_1 happy_var_2
	)}}

happyReduce_266 = happySpecReduce_2 113# happyReduction_266
happyReduction_266 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "as"
	)}

happyReduce_267 = happySpecReduce_2 113# happyReduction_267
happyReduction_267 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "export"
	)}

happyReduce_268 = happySpecReduce_2 113# happyReduction_268
happyReduction_268 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "hiding"
	)}

happyReduce_269 = happySpecReduce_2 113# happyReduction_269
happyReduction_269 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "qualified"
	)}

happyReduce_270 = happySpecReduce_2 113# happyReduction_270
happyReduction_270 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "safe"
	)}

happyReduce_271 = happySpecReduce_2 113# happyReduction_271
happyReduction_271 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn117
		 (HsIdent happy_var_1 "unsafe"
	)}

happyReduce_272 = happySpecReduce_1 114# happyReduction_272
happyReduction_272 happy_x_1
	 =  case happyOut119 happy_x_1 of { happy_var_1 -> 
	happyIn118
		 (UnQual happy_var_1
	)}

happyReduce_273 = happySpecReduce_2 114# happyReduction_273
happyReduction_273 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (QConId happy_var_2) -> 
	happyIn118
		 (Qual (Module (fst happy_var_2)) (HsIdent happy_var_1 (snd happy_var_2))
	)}}

happyReduce_274 = happySpecReduce_2 115# happyReduction_274
happyReduction_274 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (ConId happy_var_2) -> 
	happyIn119
		 (HsIdent happy_var_1 happy_var_2
	)}}

happyReduce_275 = happySpecReduce_1 116# happyReduction_275
happyReduction_275 happy_x_1
	 =  case happyOut121 happy_x_1 of { happy_var_1 -> 
	happyIn120
		 (UnQual happy_var_1
	)}

happyReduce_276 = happySpecReduce_2 116# happyReduction_276
happyReduction_276 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (QConSym happy_var_2) -> 
	happyIn120
		 (Qual (Module (fst happy_var_2)) (HsSymbol happy_var_1 (snd happy_var_2))
	)}}

happyReduce_277 = happySpecReduce_2 117# happyReduction_277
happyReduction_277 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (ConSym happy_var_2) -> 
	happyIn121
		 (HsSymbol happy_var_1 happy_var_2
	)}}

happyReduce_278 = happySpecReduce_1 118# happyReduction_278
happyReduction_278 happy_x_1
	 =  case happyOut124 happy_x_1 of { happy_var_1 -> 
	happyIn122
		 (UnQual happy_var_1
	)}

happyReduce_279 = happySpecReduce_1 118# happyReduction_279
happyReduction_279 happy_x_1
	 =  case happyOut126 happy_x_1 of { happy_var_1 -> 
	happyIn122
		 (happy_var_1
	)}

happyReduce_280 = happySpecReduce_1 119# happyReduction_280
happyReduction_280 happy_x_1
	 =  case happyOut125 happy_x_1 of { happy_var_1 -> 
	happyIn123
		 (UnQual happy_var_1
	)}

happyReduce_281 = happySpecReduce_1 119# happyReduction_281
happyReduction_281 happy_x_1
	 =  case happyOut126 happy_x_1 of { happy_var_1 -> 
	happyIn123
		 (happy_var_1
	)}

happyReduce_282 = happySpecReduce_2 120# happyReduction_282
happyReduction_282 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (VarSym happy_var_2) -> 
	happyIn124
		 (HsSymbol happy_var_1 happy_var_2
	)}}

happyReduce_283 = happySpecReduce_2 120# happyReduction_283
happyReduction_283 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn124
		 (HsSymbol happy_var_1 "-"
	)}

happyReduce_284 = happySpecReduce_2 120# happyReduction_284
happyReduction_284 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn124
		 (HsSymbol happy_var_1 "!"
	)}

happyReduce_285 = happySpecReduce_2 121# happyReduction_285
happyReduction_285 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (VarSym happy_var_2) -> 
	happyIn125
		 (HsSymbol happy_var_1 happy_var_2
	)}}

happyReduce_286 = happySpecReduce_2 121# happyReduction_286
happyReduction_286 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	happyIn125
		 (HsSymbol happy_var_1 "!"
	)}

happyReduce_287 = happySpecReduce_2 122# happyReduction_287
happyReduction_287 happy_x_2
	happy_x_1
	 =  case happyOut128 happy_x_1 of { happy_var_1 -> 
	case happyOutTok happy_x_2 of { (QVarSym happy_var_2) -> 
	happyIn126
		 (Qual (Module (fst happy_var_2)) (HsSymbol happy_var_1 (snd happy_var_2))
	)}}

happyReduce_288 = happySpecReduce_1 123# happyReduction_288
happyReduction_288 happy_x_1
	 =  case happyOutTok happy_x_1 of { (IntTok happy_var_1) -> 
	happyIn127
		 (HsInt happy_var_1
	)}

happyReduce_289 = happySpecReduce_1 123# happyReduction_289
happyReduction_289 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Character happy_var_1) -> 
	happyIn127
		 (HsChar happy_var_1
	)}

happyReduce_290 = happySpecReduce_1 123# happyReduction_290
happyReduction_290 happy_x_1
	 =  case happyOutTok happy_x_1 of { (FloatTok happy_var_1) -> 
	happyIn127
		 (HsFrac happy_var_1
	)}

happyReduce_291 = happySpecReduce_1 123# happyReduction_291
happyReduction_291 happy_x_1
	 =  case happyOutTok happy_x_1 of { (StringTok happy_var_1) -> 
	happyIn127
		 (HsString happy_var_1
	)}

happyReduce_292 = happyMonadReduce 0# 124# happyReduction_292
happyReduction_292 (happyRest)
	 = happyThen ( getSrcLoc
	) (\r -> happyReturn (happyIn128 r))

happyReduce_293 = happyMonadReduce 0# 125# happyReduction_293
happyReduction_293 (happyRest)
	 = happyThen ( pushCurrentContext
	) (\r -> happyReturn (happyIn129 r))

happyReduce_294 = happySpecReduce_1 126# happyReduction_294
happyReduction_294 happy_x_1
	 =  happyIn130
		 (()
	)

happyReduce_295 = happyMonadReduce 1# 126# happyReduction_295
happyReduction_295 (happy_x_1 `HappyStk`
	happyRest)
	 = happyThen ( popContext
	) (\r -> happyReturn (happyIn130 r))

happyReduce_296 = happySpecReduce_1 127# happyReduction_296
happyReduction_296 happy_x_1
	 =  case happyOutTok happy_x_1 of { (ConId happy_var_1) -> 
	happyIn131
		 (Module happy_var_1
	)}

happyReduce_297 = happySpecReduce_1 127# happyReduction_297
happyReduction_297 happy_x_1
	 =  case happyOutTok happy_x_1 of { (QConId happy_var_1) -> 
	happyIn131
		 (Module (fst happy_var_1 ++ '.':snd happy_var_1)
	)}

happyReduce_298 = happySpecReduce_1 128# happyReduction_298
happyReduction_298 happy_x_1
	 =  case happyOut119 happy_x_1 of { happy_var_1 -> 
	happyIn132
		 (happy_var_1
	)}

happyReduce_299 = happySpecReduce_1 129# happyReduction_299
happyReduction_299 happy_x_1
	 =  case happyOut119 happy_x_1 of { happy_var_1 -> 
	happyIn133
		 (happy_var_1
	)}

happyReduce_300 = happySpecReduce_1 130# happyReduction_300
happyReduction_300 happy_x_1
	 =  case happyOut118 happy_x_1 of { happy_var_1 -> 
	happyIn134
		 (happy_var_1
	)}

happyReduce_301 = happySpecReduce_1 131# happyReduction_301
happyReduction_301 happy_x_1
	 =  case happyOut118 happy_x_1 of { happy_var_1 -> 
	happyIn135
		 (happy_var_1
	)}

happyReduce_302 = happySpecReduce_1 132# happyReduction_302
happyReduction_302 happy_x_1
	 =  case happyOut117 happy_x_1 of { happy_var_1 -> 
	happyIn136
		 (happy_var_1
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	EOF -> happyDoAction 65# (error "reading EOF!") action sts stk;
	VarId happy_dollar_dollar -> cont 1#;
	QVarId happy_dollar_dollar -> cont 2#;
	ConId happy_dollar_dollar -> cont 3#;
	QConId happy_dollar_dollar -> cont 4#;
	VarSym happy_dollar_dollar -> cont 5#;
	ConSym happy_dollar_dollar -> cont 6#;
	QVarSym happy_dollar_dollar -> cont 7#;
	QConSym happy_dollar_dollar -> cont 8#;
	IntTok happy_dollar_dollar -> cont 9#;
	FloatTok happy_dollar_dollar -> cont 10#;
	Character happy_dollar_dollar -> cont 11#;
	StringTok happy_dollar_dollar -> cont 12#;
	LeftParen -> cont 13#;
	RightParen -> cont 14#;
	SemiColon -> cont 15#;
	LeftCurly -> cont 16#;
	RightCurly -> cont 17#;
	VRightCurly -> cont 18#;
	LeftSquare -> cont 19#;
	RightSquare -> cont 20#;
	Comma -> cont 21#;
	Underscore -> cont 22#;
	BackQuote -> cont 23#;
	DotDot -> cont 24#;
	Colon -> cont 25#;
	DoubleColon -> cont 26#;
	Equals -> cont 27#;
	Backslash -> cont 28#;
	Bar -> cont 29#;
	LeftArrow -> cont 30#;
	RightArrow -> cont 31#;
	At -> cont 32#;
	Tilde -> cont 33#;
	DoubleArrow -> cont 34#;
	Minus -> cont 35#;
	Exclamation -> cont 36#;
	KW_Case -> cont 37#;
	KW_Class -> cont 38#;
	KW_Data -> cont 39#;
	KW_Default -> cont 40#;
	KW_Deriving -> cont 41#;
	KW_Do -> cont 42#;
	KW_Else -> cont 43#;
	KW_Foreign -> cont 44#;
	KW_If -> cont 45#;
	KW_Import -> cont 46#;
	KW_In -> cont 47#;
	KW_Infix -> cont 48#;
	KW_InfixL -> cont 49#;
	KW_InfixR -> cont 50#;
	KW_Instance -> cont 51#;
	KW_Let -> cont 52#;
	KW_Module -> cont 53#;
	KW_NewType -> cont 54#;
	KW_Of -> cont 55#;
	KW_Then -> cont 56#;
	KW_Type -> cont 57#;
	KW_Where -> cont 58#;
	KW_As -> cont 59#;
	KW_Export -> cont 60#;
	KW_Hiding -> cont 61#;
	KW_Qualified -> cont 62#;
	KW_Safe -> cont 63#;
	KW_Unsafe -> cont 64#;
	_ -> happyError'
	})

happyError_ tk = happyError'

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (>>=)
happyReturn :: () => a -> P a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => P a
happyError' = happyError

parse = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq

happyError :: P a
happyError = fail "Parse error"

-- | Parse of a string, which should contain a complete Haskell 98 module.
parseModule :: String -> ParseResult HsModule
parseModule = runParser parse

-- | Parse of a string, which should contain a complete Haskell 98 module.
parseModuleWithMode :: ParseMode -> String -> ParseResult HsModule
parseModuleWithMode mode = runParserWithMode mode parse
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id$













{-# LINE 27 "GenericTemplate.hs" #-}



data Happy_IntList = HappyCons Int# Happy_IntList






































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	(happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
	= {- nothing -}


	  case action of
		0#		  -> {- nothing -}
				     happyFail i tk st
		-1# 	  -> {- nothing -}
				     happyAccept i tk st
		n | (n <# (0# :: Int#)) -> {- nothing -}

				     (happyReduceArr ! rule) i tk st
				     where rule = (I# ((negateInt# ((n +# (1# :: Int#))))))
		n		  -> {- nothing -}


				     happyShift new_state i tk st
				     where new_state = (n -# (1# :: Int#))
   where off    = indexShortOffAddr happyActOffsets st
	 off_i  = (off +# i)
	 check  = if (off_i >=# (0# :: Int#))
			then (indexShortOffAddr happyCheck off_i ==#  i)
			else False
 	 action | check     = indexShortOffAddr happyTable off_i
		| otherwise = indexShortOffAddr happyDefActions st











indexShortOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ > 500
	narrow16Int# i
#elif __GLASGOW_HASKELL__ == 500
	intToInt16# i
#else
	(i `iShiftL#` 16#) `iShiftRA#` 16#
#endif
  where
#if __GLASGOW_HASKELL__ >= 503
	i = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
#else
	i = word2Int# ((high `shiftL#` 8#) `or#` low)
#endif
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#





data HappyAddr = HappyA# Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 169 "GenericTemplate.hs" #-}


-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case unsafeCoerce# x of { (I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k -# (1# :: Int#)) sts of
	 sts1@((HappyCons (st1@(action)) (_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@((HappyCons (st1@(action)) (_))) = happyDrop k (HappyCons (st) (sts))
             drop_stk = happyDropStk k stk

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n -# (1# :: Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n -# (1#::Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off    = indexShortOffAddr happyGotoOffsets st
	 off_i  = (off +# nt)
 	 new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail  0# tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
	happyDoAction 0# tk action sts ( (unsafeCoerce# (I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
