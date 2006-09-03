// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.core.codeassist;

/** <p>contains a collection of keywords and names usually considered 
  * in syntax coloring.</p>
  * 
  * @author Leif Frenzel
  */
public final class HaskellSyntax {

  // TODO will be made configurable later from a Pref page;
  // even later we should base all of the content assist on the language model
  private static final String[] KEYWORDS = new String[] {
    "as", "case", "class", "data", "default", "deriving", "else", "hiding",
    "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
    "module", "of", "primitive", "qualified", "then", "type", "where", "do",
    "newtype"
  };

  private static final String[] FUNCTIONS = new String[] {
    "map", "concat", "filter", "head", "last", "tail", "init", "null", "length",
    "foldl", "foldl1", "scanl", "scanl1", "foldr", "foldr1", "scanr", "scanr1",
    "iterate", "repeat", "replicate", "cycle", "take", "drop", "splitAt",
    "takeWhile", "dropWhile", "span", "break", "lines", "words", "unlines", 
    "unwords", "reverse", "and", "or", "any", "all", "elem", "notElem", 
    "lookup", "sum", "product", "maximum", "minimum", "concatMap", "zip", 
    "zip3", "zipWith", "zipWith3", "unzip", "unzip3", "readsPrec", "readList",
    "show", "showsPrec", "showList", "reads", "shows", "read", "lex", 
    "showChar", "showString", "readParen", "showParen", "ioError", "userError",
    "catch", "putChar", "putStr", "putStrLn", "print", "getChar", "getLine",
    "getContents", "interact", "readFile", "writeFile", "appendFile", "readIO",
    "readLn", "range", "index", "inRange", "rangeSize", "isAscii", "isControl",
    "isPrint", "isSpace", "isUpper", "isLower", "isAlpha", "isDigit", 
    "isOctDigit", "isHexDigit", "isAlphaNum", "digitToInt", "intToDigit", 
    "toUpper", "toLower", "ord", "chr", "readLitChar", "showLitChar", 
    "lexLitChar", "showSigned", "showInt", "readSigned", "readInt", "readDec", 
    "readOct", "readHex", "readSigned", "readFloat", "lexDigits", "numerator", 
    "denominator", "approxRational", "primExitWith", "Addr", "max", "min", 
    "succ", "pred", "toEnum", "fromEnum", "enumFrom", "enumFromThen", 
    "enumFromTo", "enumFromThenTo", "minBound", "maxBound", "negate", "abs", 
    "signum", "fromInteger", "fromInt", "toRational", "quot", "rem", "div", 
    "mod", "quotRem", "divMod", "even", "odd", "toInteger", "toInt", "recip", 
    "fromRational", "fromDouble", "pi", "exp", "log", "sqrt", "logBase", "sin", 
    "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", 
    "acosh", "atanh", "properFraction", "truncate", "round", "ceiling", 
    "floor", "floatRadix", "floatDigits", "floatRange", "decodeFloat", 
    "encodeFloat", "exponent", "significand", "scaleFloat", "isNaN", 
    "isInfinite", "isDenormalized", "isIEEE", "isNegativeZero", "atan2", 
    "return", "fail", "fmap", "mapM", "mapM_", "sequence", "sequence_", 
    "maybe", "either", "not", "otherwise", "subtract", "even", "odd", "gcd", 
    "lcm", "fromIntegral", "realToFrac", "fst", "snd", "curry", "uncurry", "id",
    "const", "flip", "until", "asTypeOf", "error", "undefined", "seq", 
    "compare", "primEqChar", "primCmpChar", "primCharToInt", "primIntToChar", 
    "primCompAux", "primEqInt", "primCmpInt", "primEqInteger", "primCmpInteger",
    "primPlusInt", "primMinusInt", "primMulInt", "primNegInt", 
    "primIntegerToInt", "primMinInt", "primMaxInt", "primPlusInteger", 
    "primMinusInteger", "primMulInteger", "primNegInteger", "primIntToInteger", 
    "absReal", "signumReal", "primDivInt", "primQuotInt", "primRemInt", 
    "primModInt", "primQrmInt", "primEvenInt", "primQrmInteger",  
    "primEvenInteger", "numericEnumFrom", "numericEnumFromThen", 
    "numericEnumFromTo", "numericEnumFromThenTo", "primShowsInt", 
    "primShowsInteger", "primEqFloat", "primCmpFloat", "primEqDouble", 
    "primCmpDouble", "primPlusFloat", "primMinusFloat", "primMulFloat", 
    "primNegFloat", "primIntToFloat", "primIntegerToFloat", "primPlusDouble", 
    "primMinusDouble", "primMulDouble", "primNegDouble", "primIntToDouble", 
    "primIntegerToDouble", "floatToRational", "doubleToRational", 
    "realFloatToRational", "primDivFloat", "doubleToFloat", "primDivDouble", 
    "primRationalToDouble", "primRationalToFloat", "rationalToFloat", 
    "rationalToDouble", "rationalToRealFloat", "rationalToRealFloat", 
    "primSinFloat", "primAsinFloat", "primCosFloat", "primAcosFloat", 
    "primTanFloat", "primAtanFloat", "primLogFloat", "primExpFloat", 
    "primSqrtFloat", "primSinDouble", "primAsinDouble", "primCosDouble", 
    "primAcosDouble", "primTanDouble", "primAtanDouble", "primLogDouble", 
    "primExpDouble", "primSqrtDouble", "floatProperFraction", "primFloatRadix", 
    "primFloatDigits", "primFloatMinExp", "primFloatMaxExp", "primFloatEncode", 
    "primFloatDecode", "primDoubleRadix", "primDoubleDigits", 
    "primDoubleMinExp", "primDoubleMaxExp", "primDoubleEncode", 
    "primDoubleDecode", "primShowsFloat", "reduce", "intToRatio", 
    "doubleToRatio", "showField", "readField", "nonnull", "lexmatch", 
    "asciiTab", "protectEsc", "primbindIO", "primretIO", "hugsPutStr", 
    "hugsIORun", "ioeGetErrorString", "primPmInt", "primPmInteger", "primPmFlt",
    "primPmNpk", "primPmSub"   
  };

  private static final String[] VAL = new String[] {
    "ReadS", "ShowS", "Read", "Show", "FilePath", "IOError", "Ix", "Ratio", 
    "Rational", "IOResult", "Addr", "Bool", "Maybe", "Either", "Ordering", 
    "Char", "String", "Int", "Integer", "Float", "Double", "IO", "Rec", 
    "EmptyRec", "EmptyRow", "Prelude", "False", "True", "Nothing", "Just", 
    "Left", "Right", "LT", "EQ", "GT"
  };
  
  private static final String[] CLASSES = new String[] {
    "Eq", "Show", "Read", "Ord", "Num", "Bounded", "Enum", "Real", "Fractional",
    "Integral", "RealFrac", "Floating", "RealFloat", "Monad", "Functor"
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
