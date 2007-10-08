-- #hide
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.ParseUtils
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for the Haskell parser.
--
-----------------------------------------------------------------------------

module Language.Haskell.ParseUtils (
	  splitTyConApp		-- HsType -> P (HsName,[HsType])
	, mkRecConstrOrUpdate	-- HsExp -> [HsFieldUpdate] -> P HsExp
	, checkPrec		-- Integer -> P Int
	, checkContext		-- HsType -> P HsContext
	, checkAssertion	-- HsType -> P HsAsst
	, checkDataHeader	-- HsQualType -> P (HsContext,HsName,[HsName])
	, checkClassHeader	-- HsQualType -> P (HsContext,HsName,[HsName])
	, checkInstHeader	-- HsQualType -> P (HsContext,HsQName,[HsType])
	, checkPattern		-- HsExp -> P HsPat
	, checkExpr		-- HsExp -> P HsExp
	, checkValDef		-- SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
	, checkClassBody	-- [HsDecl] -> P [HsDecl]
	, checkUnQual		-- HsQName -> P HsName
	, checkRevDecls		-- [HsDecl] -> P [HsDecl]
 ) where

import Language.Haskell.Syntax
import Language.Haskell.ParseMonad

splitTyConApp :: HsType -> P (HsName,[HsType])
splitTyConApp t0 = split t0 []
 where
	split :: HsType -> [HsType] -> P (HsName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon (UnQual t)) ts = return (t,ts)
	split _ _ = fail "Illegal data/newtype declaration"

-----------------------------------------------------------------------------
-- Various Syntactic Checks

checkContext :: HsType -> P HsContext
checkContext (HsTyTuple ts) =
	mapM checkAssertion ts
checkContext t = do
	c <- checkAssertion t
	return [c]

-- Changed for multi-parameter type classes

checkAssertion :: HsType -> P HsAsst
checkAssertion = checkAssertion' []
	where	checkAssertion' ts (HsTyCon c) = return (c,ts)
		checkAssertion' ts (HsTyApp a t) = checkAssertion' (t:ts) a
		checkAssertion' _ _ = fail "Illegal class assertion"


checkDataHeader :: HsQualType -> P (HsContext,HsName,[HsName])
checkDataHeader (HsQualType cs t) = do
	(c,ts) <- checkSimple "data/newtype" t []
	return (cs,c,ts)

checkClassHeader :: HsQualType -> P (HsContext,HsName,[HsName])
checkClassHeader (HsQualType cs t) = do
	(c,ts) <- checkSimple "class" t []
	return (cs,c,ts)

checkSimple :: String -> HsType -> [HsName] -> P ((HsName,[HsName]))
checkSimple kw (HsTyApp l (HsTyVar a)) xs = checkSimple kw l (a:xs)
checkSimple _kw (HsTyCon (UnQual t))   xs = return (t,xs)
checkSimple kw _ _ = fail ("Illegal " ++ kw ++ " declaration")

checkInstHeader :: HsQualType -> P (HsContext,HsQName,[HsType])
checkInstHeader (HsQualType cs t) = do
	(c,ts) <- checkInsts t []
	return (cs,c,ts)

checkInsts :: HsType -> [HsType] -> P ((HsQName,[HsType]))
checkInsts (HsTyApp l t) ts = checkInsts l (t:ts)
checkInsts (HsTyCon c)   ts = return (c,ts)
checkInsts _ _ = fail "Illegal instance declaration"

-----------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: HsExp -> P HsPat
checkPattern e = checkPat e []

checkPat :: HsExp -> [HsPat] -> P HsPat
checkPat (HsCon c) args = return (HsPApp c args)
checkPat (HsApp f x) args = do
	x <- checkPat x []
	checkPat f (x:args)
checkPat e [] = case e of
	HsVar (UnQual x)   -> return (HsPVar x)
	HsLit l            -> return (HsPLit l)
	HsInfixApp l op r  -> do
			      l <- checkPat l []
			      r <- checkPat r []
			      case op of
				 HsQConOp c -> return (HsPInfixApp l c r)
				 _ -> patFail
	HsTuple es         -> do
			      ps <- mapM (\e -> checkPat e []) es
			      return (HsPTuple ps)
	HsList es	   -> do
			      ps <- mapM (\e -> checkPat e []) es
			      return (HsPList ps)
	HsParen e	   -> do
			      p <- checkPat e []
			      return (HsPParen p)
	HsAsPat n e	   -> do
			      p <- checkPat e []
			      return (HsPAsPat n p)
	HsWildCard	   -> return HsPWildCard
	HsIrrPat e	   -> do
			      p <- checkPat e []
			      return (HsPIrrPat p)
	HsRecConstr c fs   -> do
			      fs <- mapM checkPatField fs
			      return (HsPRec c fs)
	HsNegApp (HsLit l) -> return (HsPNeg (HsPLit l))
	_ -> patFail

checkPat _ _ = patFail

checkPatField :: HsFieldUpdate -> P HsPatField
checkPatField (HsFieldUpdate n e) = do
	p <- checkPat e []
	return (HsPFieldPat n p)

patFail :: P a
patFail = fail "Parse error in pattern"

-----------------------------------------------------------------------------
-- Check Expression Syntax

checkExpr :: HsExp -> P HsExp
checkExpr e = case e of
	HsVar _			  -> return e
	HsCon _			  -> return e
	HsLit _			  -> return e
	HsInfixApp e1 op e2	  -> check2Exprs e1 e2 (flip HsInfixApp op)
	HsApp e1 e2		  -> check2Exprs e1 e2 HsApp
	HsNegApp e		  -> check1Expr e HsNegApp
	HsLambda loc ps e	  -> check1Expr e (HsLambda loc ps)
	HsLet bs e		  -> check1Expr e (HsLet bs)
	HsIf e1 e2 e3		  -> check3Exprs e1 e2 e3 HsIf
	HsCase e alts		  -> do
				     alts <- mapM checkAlt alts
				     e <- checkExpr e
				     return (HsCase e alts)
	HsDo stmts		  -> do
				     stmts <- mapM checkStmt stmts
				     return (HsDo stmts)
	HsTuple es		  -> checkManyExprs es HsTuple
	HsList es		  -> checkManyExprs es HsList
	HsParen e		  -> check1Expr e HsParen
	HsLeftSection e op	  -> check1Expr e (flip HsLeftSection op)
	HsRightSection op e	  -> check1Expr e (HsRightSection op)
	HsRecConstr c fields	  -> do
				     fields <- mapM checkField fields
				     return (HsRecConstr c fields)
	HsRecUpdate e fields	  -> do
				     fields <- mapM checkField fields
				     e <- checkExpr e
				     return (HsRecUpdate e fields)
	HsEnumFrom e		  -> check1Expr e HsEnumFrom
	HsEnumFromTo e1 e2	  -> check2Exprs e1 e2 HsEnumFromTo
	HsEnumFromThen e1 e2      -> check2Exprs e1 e2 HsEnumFromThen
	HsEnumFromThenTo e1 e2 e3 -> check3Exprs e1 e2 e3 HsEnumFromThenTo
	HsListComp e stmts        -> do
				     stmts <- mapM checkStmt stmts
				     e <- checkExpr e
				     return (HsListComp e stmts)
	HsExpTypeSig loc e ty     -> do
				     e <- checkExpr e
				     return (HsExpTypeSig loc e ty)
	_                         -> fail "Parse error in expression"

-- type signature for polymorphic recursion!!
check1Expr :: HsExp -> (HsExp -> a) -> P a
check1Expr e1 f = do
	e1 <- checkExpr e1
	return (f e1)

check2Exprs :: HsExp -> HsExp -> (HsExp -> HsExp -> a) -> P a
check2Exprs e1 e2 f = do
	e1 <- checkExpr e1
	e2 <- checkExpr e2
	return (f e1 e2)

check3Exprs :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> a) -> P a
check3Exprs e1 e2 e3 f = do
	e1 <- checkExpr e1
	e2 <- checkExpr e2
	e3 <- checkExpr e3
	return (f e1 e2 e3)

checkManyExprs :: [HsExp] -> ([HsExp] -> a) -> P a
checkManyExprs es f = do
	es <- mapM checkExpr es
	return (f es)

checkAlt :: HsAlt -> P HsAlt
checkAlt (HsAlt loc p galts bs) = do
	galts <- checkGAlts galts
	return (HsAlt loc p galts bs)

checkGAlts :: HsGuardedAlts -> P HsGuardedAlts
checkGAlts (HsUnGuardedAlt e) = check1Expr e HsUnGuardedAlt
checkGAlts (HsGuardedAlts galts) = do
	galts <- mapM checkGAlt galts
	return (HsGuardedAlts galts)

checkGAlt :: HsGuardedAlt -> P HsGuardedAlt
checkGAlt (HsGuardedAlt loc e1 e2) = check2Exprs e1 e2 (HsGuardedAlt loc)

checkStmt :: HsStmt -> P HsStmt
checkStmt (HsGenerator loc p e) = check1Expr e (HsGenerator loc p)
checkStmt (HsQualifier e) = check1Expr e HsQualifier
checkStmt s@(HsLetStmt _) = return s

checkField :: HsFieldUpdate -> P HsFieldUpdate
checkField (HsFieldUpdate n e) = check1Expr e (HsFieldUpdate n)

-----------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcLoc -> HsExp -> HsRhs -> [HsDecl] -> P HsDecl
checkValDef srcloc lhs rhs whereBinds =
    case isFunLhs lhs [] of
	 Just (f,es) -> do
			ps <- mapM checkPattern es
			return (HsFunBind [HsMatch srcloc f ps rhs whereBinds])
         Nothing     -> do
			lhs <- checkPattern lhs
			return (HsPatBind srcloc lhs rhs whereBinds)

-- A variable binding is parsed as an HsPatBind.

isFunLhs :: HsExp -> [HsExp] -> Maybe (HsName, [HsExp])
isFunLhs (HsInfixApp l (HsQVarOp (UnQual op)) r) es = Just (op, l:r:es)
isFunLhs (HsApp (HsVar (UnQual f)) e) es = Just (f, e:es)
isFunLhs (HsApp (HsParen f) e) es = isFunLhs f (e:es)
isFunLhs (HsApp f e) es = isFunLhs f (e:es)
isFunLhs _ _ = Nothing

-----------------------------------------------------------------------------
-- In a class or instance body, a pattern binding must be of a variable.

checkClassBody :: [HsDecl] -> P [HsDecl]
checkClassBody decls = do
	mapM_ checkMethodDef decls
	return decls

checkMethodDef :: HsDecl -> P ()
checkMethodDef (HsPatBind _ (HsPVar _) _ _) = return ()
checkMethodDef (HsPatBind loc _ _ _) =
	fail "illegal method definition" `atSrcLoc` loc
checkMethodDef _ = return ()

-----------------------------------------------------------------------------
-- Check that an identifier or symbol is unqualified.
-- For occasions when doing this in the grammar would cause conflicts.

checkUnQual :: HsQName -> P HsName
checkUnQual (Qual _ _) = fail "Illegal qualified name"
checkUnQual (UnQual n) = return n
checkUnQual (Special _) = fail "Illegal special name"

-----------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrec :: Integer -> P Int
checkPrec i | 0 <= i && i <= 9 = return (fromInteger i)
checkPrec i | otherwise	       = fail ("Illegal precedence " ++ show i)

mkRecConstrOrUpdate :: HsExp -> [HsFieldUpdate] -> P HsExp
mkRecConstrOrUpdate (HsCon c) fs       = return (HsRecConstr c fs)
mkRecConstrOrUpdate e         fs@(_:_) = return (HsRecUpdate e fs)
mkRecConstrOrUpdate _         _        = fail "Empty record update"

-----------------------------------------------------------------------------
-- Reverse a list of declarations, merging adjacent HsFunBinds of the
-- same name and checking that their arities match.

checkRevDecls :: [HsDecl] -> P [HsDecl]
checkRevDecls = mergeFunBinds []
    where
	mergeFunBinds revDs [] = return revDs
	mergeFunBinds revDs (HsFunBind ms1@(HsMatch _ name ps _ _:_):ds1) =
		mergeMatches ms1 ds1
	    where
		arity = length ps
		mergeMatches ms' (HsFunBind ms@(HsMatch loc name' ps' _ _:_):ds)
		    | name' == name =
			if length ps' /= arity
			then fail ("arity mismatch for '" ++ (show name) ++ "'")
			     `atSrcLoc` loc
			else mergeMatches (ms++ms') ds
		mergeMatches ms' ds = mergeFunBinds (HsFunBind ms':revDs) ds
	mergeFunBinds revDs (d:ds) = mergeFunBinds (d:revDs) ds
