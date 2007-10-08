{-# OPTIONS_GHC -fglasgow-exts -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Syntax
-- Copyright   :  (c) The GHC Team, 1997-2000
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- A suite of datatypes describing the abstract syntax of Haskell 98
-- <http://www.haskell.org/onlinereport/> plus a few extensions:
--
--   * multi-parameter type classes
--
--   * parameters of type class assertions are unrestricted
-- 
-- This module has been changed so that show is a real show.
-- For GHC, we also derive Typeable and Data for all types.

-----------------------------------------------------------------------------

module Language.Haskell.Syntax (
    -- * Modules
    HsModule(..), HsExportSpec(..),
    HsImportDecl(..), HsImportSpec(..), HsAssoc(..),
    -- * Declarations
    HsDecl(..), HsConDecl(..), HsBangType(..),
    HsMatch(..), HsRhs(..), HsGuardedRhs(..),
    HsSafety(..),
    -- * Class Assertions and Contexts
    HsQualType(..), HsContext, HsAsst,
    -- * Types
    HsType(..),
    -- * Expressions
    HsExp(..), HsStmt(..), HsFieldUpdate(..),
    HsAlt(..), HsGuardedAlts(..), HsGuardedAlt(..),
    -- * Patterns
    HsPat(..), HsPatField(..),
    -- * Literals
    HsLiteral(..),
    -- * Variables, Constructors and Operators
    Module(..), HsQName(..), HsName(..), HsQOp(..), HsOp(..),
    HsSpecialCon(..), HsCName(..),

    -- * Builtin names

    -- ** Modules
    prelude_mod, main_mod,
    -- ** Main function of a program
    main_name,
    -- ** Constructors
    unit_con_name, tuple_con_name, list_cons_name,
    unit_con, tuple_con,
    -- ** Type constructors
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,

    -- * Source coordinates
    SrcLoc(..),
  ) where


#ifdef __GLASGOW_HASKELL__
import Data.Generics.Basics
import Data.Generics.Instances()
#endif

-- | A position in the source.
data SrcLoc = SrcLoc {
		srcFilename :: String,
		srcLine :: Int,
		srcColumn :: Int
		}
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | The name of a Haskell module.
newtype Module = Module String
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Constructors with special syntax.
-- These names are never qualified, and always refer to builtin type or
-- data constructors.

data HsSpecialCon
	= HsUnitCon		-- ^ unit type and data constructor @()@
	| HsListCon		-- ^ list type constructor @[]@
	| HsFunCon		-- ^ function type constructor @->@
	| HsTupleCon Int	-- ^ /n/-ary tuple type and data
				--   constructors @(,)@ etc
	| HsCons		-- ^ list data constructor @(:)@
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent qualified variables, and also
-- qualified constructors.
data HsQName
	= Qual Module HsName	-- ^ name qualified with a module name
	| UnQual HsName		-- ^ unqualified name
	| Special HsSpecialCon	-- ^ built-in constructor with special syntax
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | This type is used to represent variables, and also constructors.
data HsName
	= HsIdent SrcLoc String	-- ^ /varid/ or /conid/
	| HsSymbol SrcLoc String	-- ^ /varsym/ or /consym/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Possibly qualified infix operators (/qop/), appearing in expressions.
data HsQOp
	= HsQVarOp HsQName	-- ^ variable operator (/qvarop/)
	| HsQConOp HsQName	-- ^ constructor operator (/qconop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | Operators, appearing in @infix@ declarations.
data HsOp
	= HsVarOp HsName	-- ^ variable operator (/varop/)
	| HsConOp HsName	-- ^ constructor operator (/conop/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A name (/cname/) of a component of a class or data type in an @import@
-- or export specification.
data HsCName
	= HsVarName HsName	-- ^ name of a method or field
	| HsConName HsName	-- ^ name of a data constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A Haskell source module.
data HsModule = HsModule SrcLoc Module (Maybe [HsExportSpec])
                         [HsImportDecl] [HsDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Show,Typeable,Data)
#else
  deriving (Show)
#endif

-- | Export specification.
data HsExportSpec
	 = HsEVar HsQName			-- ^ variable
	 | HsEAbs HsQName			-- ^ @T@:
			-- a class or datatype exported abstractly,
			-- or a type synonym.
	 | HsEThingAll HsQName			-- ^ @T(..)@:
			-- a class exported with all of its methods, or
			-- a datatype exported with all of its constructors.
	 | HsEThingWith HsQName [HsCName]	-- ^ @T(C_1,...,C_n)@:
			-- a class exported with some of its methods, or
			-- a datatype exported with some of its constructors.
	 | HsEModuleContents Module		-- ^ @module M@:
			-- re-export a module.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import declaration.
data HsImportDecl = HsImportDecl
	{ importLoc :: SrcLoc		-- ^ position of the @import@ keyword.
	, importModule :: Module	-- ^ name of the module imported.
	, importQualified :: Bool	-- ^ imported @qualified@?
	, importAs :: Maybe Module	-- ^ optional alias name in an
					-- @as@ clause.
	, importSpecs :: Maybe (Bool,[HsImportSpec])
			-- ^ optional list of import specifications.
			-- The 'Bool' is 'True' if the names are excluded
			-- by @hiding@.
	}
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Import specification.
data HsImportSpec
	 = HsIVar HsName			-- ^ variable
	 | HsIAbs HsName			-- ^ @T@:
			-- the name of a class, datatype or type synonym.
	 | HsIThingAll HsName			-- ^ @T(..)@:
			-- a class imported with all of its methods, or
			-- a datatype imported with all of its constructors.
	 | HsIThingWith HsName [HsCName]	-- ^ @T(C_1,...,C_n)@:
			-- a class imported with some of its methods, or
			-- a datatype imported with some of its constructors.
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Associativity of an operator.
data HsAssoc
	 = HsAssocNone	-- ^ non-associative operator (declared with @infix@)
	 | HsAssocLeft	-- ^ left-associative operator (declared with @infixl@).
	 | HsAssocRight	-- ^ right-associative operator (declared with @infixr@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsDecl
	 = HsTypeDecl	 SrcLoc HsName [HsName] HsType
	 | HsDataDecl	 SrcLoc HsContext HsName [HsName] [HsConDecl] [HsQName]
	 | HsInfixDecl   SrcLoc HsAssoc Int [HsOp]
	 | HsNewTypeDecl SrcLoc HsContext HsName [HsName] HsConDecl [HsQName]
	 | HsClassDecl	 SrcLoc HsContext HsName [HsName] [HsDecl]
	 | HsInstDecl	 SrcLoc HsContext HsQName [HsType] [HsDecl]
	 | HsDefaultDecl SrcLoc [HsType]
	 | HsTypeSig	 SrcLoc [HsName] HsQualType
	 | HsFunBind     [HsMatch]
	 | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
	 | HsForeignImport SrcLoc String HsSafety String HsName HsType
	 | HsForeignExport SrcLoc String String HsName HsType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Clauses of a function binding.
data HsMatch
	 = HsMatch SrcLoc HsName [HsPat] HsRhs {-where-} [HsDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Declaration of a data constructor.
data HsConDecl
	 = HsConDecl SrcLoc HsName [HsBangType]
				-- ^ ordinary data constructor
	 | HsRecDecl SrcLoc HsName [([HsName],HsBangType)]
				-- ^ record constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The type of a constructor argument or field, optionally including
-- a strictness annotation.
data HsBangType
	 = HsBangedTy   HsType	-- ^ strict component, marked with \"@!@\"
	 | HsUnBangedTy HsType	-- ^ non-strict component
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | The right hand side of a function or pattern binding.
data HsRhs
	 = HsUnGuardedRhs HsExp	-- ^ unguarded right hand side (/exp/)
	 | HsGuardedRhss  [HsGuardedRhs]
				-- ^ guarded right hand side (/gdrhs/)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded right hand side @|@ /exp/ @=@ /exp/.
-- The first expression will be Boolean-valued.
data HsGuardedRhs
	 = HsGuardedRhs SrcLoc HsExp HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- ^ Safety level for invoking a foreign entity
data HsSafety
	= HsSafe	-- ^ call may generate callbacks
	| HsUnsafe	-- ^ call will not generate callbacks
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Ord,Show,Typeable,Data)
#else
  deriving (Eq,Ord,Show)
#endif

-- | A type qualified with a context.
--   An unqualified type has an empty context.
data HsQualType
	 = HsQualType HsContext HsType
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Haskell types and type constructors.
data HsType
	 = HsTyFun   HsType HsType	-- ^ function type
	 | HsTyTuple [HsType]		-- ^ tuple type
	 | HsTyApp   HsType HsType	-- ^ application of a type constructor
	 | HsTyVar   HsName		-- ^ type variable
	 | HsTyCon   HsQName		-- ^ named type or type constructor
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

type HsContext = [HsAsst]

-- | Class assertions.
--   In Haskell 98, the argument would be a /tyvar/, but this definition
--   allows multiple parameters, and allows them to be /type/s.
type HsAsst    = (HsQName,[HsType])

-- | /literal/.
-- Values of this type hold the abstract value of the literal, not the
-- precise string representation used.  For example, @10@, @0o12@ and @0xa@
-- have the same representation.
data HsLiteral
	= HsChar	Char		-- ^ character literal
	| HsString	String		-- ^ string literal
	| HsInt		Integer		-- ^ integer literal
	| HsFrac	Rational	-- ^ floating point literal
	| HsCharPrim	Char		-- ^ GHC unboxed character literal
	| HsStringPrim	String		-- ^ GHC unboxed string literal
	| HsIntPrim	Integer		-- ^ GHC unboxed integer literal
	| HsFloatPrim	Rational	-- ^ GHC unboxed float literal
	| HsDoublePrim	Rational	-- ^ GHC unboxed double literal
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | Haskell expressions.
--
-- /Notes:/
--
-- * Because it is difficult for parsers to distinguish patterns from
--   expressions, they typically parse them in the same way and then check
--   that they have the appropriate form.  Hence the expression type
--   includes some forms that are found only in patterns.  After these
--   checks, these constructors should not be used.
--
-- * The parser does not take precedence and associativity into account,
--   so it will leave 'HsInfixApp's associated to the left.
--
-- * The 'Language.Haskell.Pretty.Pretty' instance for 'HsExp' does not
--   add parentheses in printing.

data HsExp
	= HsVar HsQName			-- ^ variable
	| HsCon HsQName			-- ^ data constructor
	| HsLit HsLiteral		-- ^ literal constant
	| HsInfixApp HsExp HsQOp HsExp	-- ^ infix application
	| HsApp HsExp HsExp		-- ^ ordinary application
	| HsNegApp HsExp		-- ^ negation expression @-@ /exp/
	| HsLambda SrcLoc [HsPat] HsExp -- ^ lambda expression
	| HsLet [HsDecl] HsExp		-- ^ local declarations with @let@
	| HsIf HsExp HsExp HsExp	-- ^ @if@ /exp/ @then@ /exp/ @else@ /exp/
	| HsCase HsExp [HsAlt]		-- ^ @case@ /exp/ @of@ /alts/
	| HsDo [HsStmt]			-- ^ @do@-expression:
					-- the last statement in the list
					-- should be an expression.
	| HsTuple [HsExp]		-- ^ tuple expression
	| HsList [HsExp]		-- ^ list expression
	| HsParen HsExp			-- ^ parenthesized expression
	| HsLeftSection HsExp HsQOp	-- ^ left section @(@/exp/ /qop/@)@
	| HsRightSection HsQOp HsExp	-- ^ right section @(@/qop/ /exp/@)@
	| HsRecConstr HsQName [HsFieldUpdate]
					-- ^ record construction expression
	| HsRecUpdate HsExp [HsFieldUpdate]
					-- ^ record update expression
	| HsEnumFrom HsExp		-- ^ unbounded arithmetic sequence,
					-- incrementing by 1
	| HsEnumFromTo HsExp HsExp	-- ^ bounded arithmetic sequence,
					-- incrementing by 1
	| HsEnumFromThen HsExp HsExp	-- ^ unbounded arithmetic sequence,
					-- with first two elements given
	| HsEnumFromThenTo HsExp HsExp HsExp
					-- ^ bounded arithmetic sequence,
					-- with first two elements given
	| HsListComp HsExp [HsStmt]	-- ^ list comprehension
	| HsExpTypeSig SrcLoc HsExp HsQualType
					-- ^ expression type signature
	| HsAsPat HsName HsExp		-- ^ patterns only
	| HsWildCard			-- ^ patterns only
	| HsIrrPat HsExp		-- ^ patterns only
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A pattern, to be matched against a value.
data HsPat
	= HsPVar HsName			-- ^ variable
	| HsPLit HsLiteral		-- ^ literal constant
	| HsPNeg HsPat			-- ^ negated pattern
	| HsPInfixApp HsPat HsQName HsPat
					-- ^ pattern with infix data constructor
	| HsPApp HsQName [HsPat]	-- ^ data constructor and argument
					-- patterns
	| HsPTuple [HsPat]		-- ^ tuple pattern
	| HsPList [HsPat]		-- ^ list pattern
	| HsPParen HsPat		-- ^ parenthesized pattern
	| HsPRec HsQName [HsPatField]	-- ^ labelled pattern
	| HsPAsPat HsName HsPat		-- ^ @\@@-pattern
	| HsPWildCard			-- ^ wildcard pattern (@_@)
	| HsPIrrPat HsPat		-- ^ irrefutable pattern (@~@)
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fpat/ in a labeled record pattern.
data HsPatField
	= HsPFieldPat HsQName HsPat
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | This type represents both /stmt/ in a @do@-expression,
--   and /qual/ in a list comprehension.
data HsStmt
	= HsGenerator SrcLoc HsPat HsExp
				-- ^ a generator /pat/ @<-@ /exp/
	| HsQualifier HsExp	-- ^ an /exp/ by itself: in a @do@-expression,
				-- an action whose result is discarded;
				-- in a list comprehension, a guard expression
	| HsLetStmt [HsDecl]	-- ^ local bindings
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /fbind/ in a labeled record construction or update expression.
data HsFieldUpdate
	= HsFieldUpdate HsQName HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | An /alt/ in a @case@ expression.
data HsAlt
	= HsAlt SrcLoc HsPat HsGuardedAlts [HsDecl]
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

data HsGuardedAlts
	= HsUnGuardedAlt HsExp		-- ^ @->@ /exp/
	| HsGuardedAlts  [HsGuardedAlt]	-- ^ /gdpat/
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-- | A guarded alternative @|@ /exp/ @->@ /exp/.
-- The first expression will be Boolean-valued.
data HsGuardedAlt
	= HsGuardedAlt SrcLoc HsExp HsExp
#ifdef __GLASGOW_HASKELL__
  deriving (Eq,Show,Typeable,Data)
#else
  deriving (Eq,Show)
#endif

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: Module
prelude_mod	      = Module "Prelude"
main_mod	      = Module "Main"

main_name :: SrcLoc -> HsName
main_name loc = HsIdent loc "main"

unit_con_name :: HsQName
unit_con_name	      = Special HsUnitCon

tuple_con_name :: Int -> HsQName
tuple_con_name i      = Special (HsTupleCon (i+1))

list_cons_name :: HsQName
list_cons_name	      = Special HsCons

unit_con :: HsExp
unit_con	      = HsCon unit_con_name

tuple_con :: Int -> HsExp
tuple_con i	      = HsCon (tuple_con_name i)

unit_tycon_name, fun_tycon_name, list_tycon_name :: HsQName
unit_tycon_name       = unit_con_name
fun_tycon_name        = Special HsFunCon
list_tycon_name       = Special HsListCon

tuple_tycon_name :: Int -> HsQName
tuple_tycon_name i    = tuple_con_name i

unit_tycon, fun_tycon, list_tycon :: HsType
unit_tycon	      = HsTyCon unit_tycon_name
fun_tycon	      = HsTyCon fun_tycon_name
list_tycon	      = HsTyCon list_tycon_name

tuple_tycon :: Int -> HsType
tuple_tycon i	      = HsTyCon (tuple_tycon_name i)
