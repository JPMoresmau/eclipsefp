// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.codeassist;

/** <p>contains a collection of keywords and names usually considered
  * in syntax coloring.</p>
  *
  * @author Leif Frenzel
  */
public final class HaskellSyntax {

  private static final String[] KEYWORDS = new String[] {
    "as", "case", "class", "data", "default", "deriving", "else", "hiding", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "if", "import", "in", "infix", "infixl", "infixr", "instance", "let", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "module", "of", "primitive", "qualified", "then", "type", "where", "do", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "newtype" //$NON-NLS-1$
  };


  private static final String[] FUNCTIONS = new String[] {
    "map", "concat", "filter", "head", "last", "tail", "init", "null", "length", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
    "foldl", "foldl1", "scanl", "scanl1", "foldr", "foldr1", "scanr", "scanr1", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "iterate", "repeat", "replicate", "cycle", "take", "drop", "splitAt", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "takeWhile", "dropWhile", "span", "break", "lines", "words", "unlines", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "unwords", "reverse", "and", "or", "any", "all", "elem", "notElem", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "lookup", "sum", "product", "maximum", "minimum", "concatMap", "zip", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "zip3", "zipWith", "zipWith3", "unzip", "unzip3", "readsPrec", "readList", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "show", "showsPrec", "showList", "reads", "shows", "read", "lex", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "showChar", "showString", "readParen", "showParen", "ioError", "userError", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "catch", "putChar", "putStr", "putStrLn", "print", "getChar", "getLine", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "getContents", "interact", "readFile", "writeFile", "appendFile", "readIO", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "readLn", "range", "index", "inRange", "rangeSize", "isAscii", "isControl", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "isPrint", "isSpace", "isUpper", "isLower", "isAlpha", "isDigit", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "isOctDigit", "isHexDigit", "isAlphaNum", "digitToInt", "intToDigit", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "toUpper", "toLower", "ord", "chr", "readLitChar", "showLitChar", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "lexLitChar", "showSigned", "showInt", "readSigned", "readInt", "readDec", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "readOct", "readHex", "readSigned", "readFloat", "lexDigits", "numerator", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "denominator", "approxRational", "primExitWith", "Addr", "max", "min", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "succ", "pred", "toEnum", "fromEnum", "enumFrom", "enumFromThen", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "enumFromTo", "enumFromThenTo", "minBound", "maxBound", "negate", "abs", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "signum", "fromInteger", "fromInt", "toRational", "quot", "rem", "div", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "mod", "quotRem", "divMod", "even", "odd", "toInteger", "toInt", "recip", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "fromRational", "fromDouble", "pi", "exp", "log", "sqrt", "logBase", "sin", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
    "acosh", "atanh", "properFraction", "truncate", "round", "ceiling", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
    "floor", "floatRadix", "floatDigits", "floatRange", "decodeFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "encodeFloat", "exponent", "significand", "scaleFloat", "isNaN", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "isInfinite", "isDenormalized", "isIEEE", "isNegativeZero", "atan2", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "return", "fail", "fmap", "mapM", "mapM_", "sequence", "sequence_", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "maybe", "either", "not", "otherwise", "subtract", "even", "odd", "gcd", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "lcm", "fromIntegral", "realToFrac", "fst", "snd", "curry", "uncurry", "id", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "const", "flip", "until", "asTypeOf", "error", "undefined", "seq", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "compare", "primEqChar", "primCmpChar", "primCharToInt", "primIntToChar", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "primCompAux", "primEqInt", "primCmpInt", "primEqInteger", "primCmpInteger", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "primPlusInt", "primMinusInt", "primMulInt", "primNegInt", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primIntegerToInt", "primMinInt", "primMaxInt", "primPlusInteger", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primMinusInteger", "primMulInteger", "primNegInteger", "primIntToInteger", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "absReal", "signumReal", "primDivInt", "primQuotInt", "primRemInt", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "primModInt", "primQrmInt", "primEvenInt", "primQrmInteger", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primEvenInteger", "numericEnumFrom", "numericEnumFromThen", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "numericEnumFromTo", "numericEnumFromThenTo", "primShowsInt", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "primShowsInteger", "primEqFloat", "primCmpFloat", "primEqDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primCmpDouble", "primPlusFloat", "primMinusFloat", "primMulFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primNegFloat", "primIntToFloat", "primIntegerToFloat", "primPlusDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primMinusDouble", "primMulDouble", "primNegDouble", "primIntToDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primIntegerToDouble", "floatToRational", "doubleToRational", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "realFloatToRational", "primDivFloat", "doubleToFloat", "primDivDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primRationalToDouble", "primRationalToFloat", "rationalToFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "rationalToDouble", "rationalToRealFloat", "rationalToRealFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "primSinFloat", "primAsinFloat", "primCosFloat", "primAcosFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primTanFloat", "primAtanFloat", "primLogFloat", "primExpFloat", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primSqrtFloat", "primSinDouble", "primAsinDouble", "primCosDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primAcosDouble", "primTanDouble", "primAtanDouble", "primLogDouble", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primExpDouble", "primSqrtDouble", "floatProperFraction", "primFloatRadix", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primFloatDigits", "primFloatMinExp", "primFloatMaxExp", "primFloatEncode", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "primFloatDecode", "primDoubleRadix", "primDoubleDigits", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "primDoubleMinExp", "primDoubleMaxExp", "primDoubleEncode", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    "primDoubleDecode", "primShowsFloat", "reduce", "intToRatio", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
    "doubleToRatio", "showField", "readField", "nonnull", "lexmatch", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "asciiTab", "protectEsc", "primbindIO", "primretIO", "hugsPutStr", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "hugsIORun", "ioeGetErrorString", "primPmInt", "primPmInteger", "primPmFlt", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
    "primPmNpk", "primPmSub" //$NON-NLS-1$ //$NON-NLS-2$
  };

  private static final String[] VAL = new String[] {
    "ReadS", "ShowS", "Read", "Show", "FilePath", "IOError", "Ix", "Ratio", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "Rational", "IOResult", "Addr", "Bool", "Maybe", "Either", "Ordering", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "Char", "String", "Int", "Integer", "Float", "Double", "IO", "Rec", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$
    "EmptyRec", "EmptyRow", "Prelude", "False", "True", "Nothing", "Just", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$
    "Left", "Right", "LT", "EQ", "GT" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$
  };

  private static final String[] CLASSES = new String[] {
    "Eq", "Show", "Read", "Ord", "Num", "Bounded", "Enum", "Real", "Fractional", //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$ //$NON-NLS-7$ //$NON-NLS-8$ //$NON-NLS-9$
    "Integral", "RealFrac", "Floating", "RealFloat", "Monad", "Functor" //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$ //$NON-NLS-6$
  };

  private HaskellSyntax() {
    // prevent instantiation
  }

  public static String[] getFunctions() {
    return FUNCTIONS;
  }

  public static String[] getLibs() {
    return VAL;
  }

  public static String[] getClasses() {
    return CLASSES;
  }

  public static String[] getKeywords() {
    return KEYWORDS;
  }


}
