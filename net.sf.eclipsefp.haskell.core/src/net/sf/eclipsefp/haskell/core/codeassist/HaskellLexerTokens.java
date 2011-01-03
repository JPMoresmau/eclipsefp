package net.sf.eclipsefp.haskell.core.codeassist;

/**
 * Haskell lexer tokens, returned by the scion-server's "tokens" and token-related commands. Note that this source
 * comes straight from the GHC Lexer.hs source, which is current as of GHC 7.0. There may be tokens that are no
 * longer used, and, consequently, no longer documented in previous sources.
 *
 * @author B. Scott Michel
 */
public class HaskellLexerTokens {
  // Keywords
  public final static String ITas = "ITas"; //$NON-NLS-1$
  public final static String ITcase = "ITcase"; //$NON-NLS-1$
  public final static String ITclass = "ITclass"; //$NON-NLS-1$
  public final static String ITdata = "ITdata"; //$NON-NLS-1$
  public final static String ITdefault = "ITdefault"; //$NON-NLS-1$
  public final static String ITderiving = "ITderiving"; //$NON-NLS-1$
  public final static String ITdo = "ITdo"; //$NON-NLS-1$
  public final static String ITelse = "ITelse"; //$NON-NLS-1$
  public final static String IThiding = "IThiding"; //$NON-NLS-1$
  public final static String ITif = "ITif"; //$NON-NLS-1$
  public final static String ITimport = "ITimport"; //$NON-NLS-1$
  public final static String ITin = "ITin"; //$NON-NLS-1$
  public final static String ITinfix = "ITinfix"; //$NON-NLS-1$
  public final static String ITinfixl = "ITinfixl"; //$NON-NLS-1$
  public final static String ITinfixr = "ITinfixr"; //$NON-NLS-1$
  public final static String ITinstance = "ITinstance"; //$NON-NLS-1$
  public final static String ITlet = "ITlet"; //$NON-NLS-1$
  public final static String ITmodule = "ITmodule"; //$NON-NLS-1$
  public final static String ITnewtype = "ITnewtype"; //$NON-NLS-1$
  public final static String ITof = "ITof"; //$NON-NLS-1$
  public final static String ITqualified = "ITqualified"; //$NON-NLS-1$
  public final static String ITthen = "ITthen"; //$NON-NLS-1$
  public final static String ITtype = "ITtype"; //$NON-NLS-1$
  public final static String ITwhere = "ITwhere"; //$NON-NLS-1$
  public final static String ITscc = "ITscc"; //$NON-NLS-1$

  public final static String ITforall = "ITforall"; //$NON-NLS-1$
  public final static String ITforeign = "ITforeign"; //$NON-NLS-1$
  public final static String ITexport = "ITexport"; //$NON-NLS-1$
  public final static String ITlabel = "ITlabel"; //$NON-NLS-1$
  public final static String ITdynamic = "ITdynamic"; //$NON-NLS-1$
  public final static String ITsafe = "ITsafe"; //$NON-NLS-1$
  public final static String ITthreadsafe = "ITthreadsafe"; //$NON-NLS-1$
  public final static String ITunsafe = "ITunsafe"; //$NON-NLS-1$
  public final static String ITstdcallconv = "ITstdcallconv"; //$NON-NLS-1$
  public final static String ITccallconv = "ITccallconv"; //$NON-NLS-1$
  public final static String ITprimcallconv = "ITprimcallconv"; //$NON-NLS-1$
  public final static String ITmdo = "ITmdo"; //$NON-NLS-1$
  public final static String ITfamily = "ITfamily"; //$NON-NLS-1$
  public final static String ITgroup = "ITgroup"; //$NON-NLS-1$
  public final static String ITby = "ITby"; //$NON-NLS-1$
  public final static String ITusing = "ITusing"; //$NON-NLS-1$

  // Pragmas
  public final static String ITinline_prag = "ITinline_prag"; //$NON-NLS-1$
  public final static String ITspec_prag  = "ITspec_prag"; //$NON-NLS-1$
  public final static String ITspec_inline_prag = "ITspec_inline_prag"; //$NON-NLS-1$
  public final static String ITsource_prag = "ITsource_prag"; //$NON-NLS-1$
  public final static String ITrules_prag = "ITrules_prag"; //$NON-NLS-1$
  public final static String ITwarning_prag = "ITwarning_prag"; //$NON-NLS-1$
  public final static String ITdeprecated_prag = "ITdeprecated_prag"; //$NON-NLS-1$
  public final static String ITline_prag = "ITline_prag"; //$NON-NLS-1$
  public final static String ITscc_prag = "ITscc_prag"; //$NON-NLS-1$
  public final static String ITgenerated_prag = "ITgenerated_prag"; //$NON-NLS-1$
  public final static String ITcore_prag = "ITcore_prag"; //$NON-NLS-1$
  public final static String ITunpack_prag = "ITunpack_prag"; //$NON-NLS-1$
  public final static String ITann_prag = "ITann_prag"; //$NON-NLS-1$
  public final static String ITclose_prag = "ITclose_prag"; //$NON-NLS-1$
  public final static String IToptions_prag = "IToptions_prag"; //$NON-NLS-1$
  public final static String ITinclude_prag = "ITinclude_prag"; //$NON-NLS-1$
  public final static String ITlanguage_prag = "ITlanguage_prag"; //$NON-NLS-1$

  public final static String ITdotdot = "ITdotdot"; //$NON-NLS-1$
  public final static String ITcolon = "ITcolon"; //$NON-NLS-1$
  public final static String ITdcolon = "ITdcolon"; //$NON-NLS-1$
  public final static String ITequal = "ITequal"; //$NON-NLS-1$
  public final static String ITlam = "ITlam"; //$NON-NLS-1$
  public final static String ITvbar = "ITvbar"; //$NON-NLS-1$
  public final static String ITlarrow = "ITlarrow"; //$NON-NLS-1$
  public final static String ITrarrow = "ITrarrow"; //$NON-NLS-1$
  public final static String ITat = "ITat"; //$NON-NLS-1$
  public final static String ITtilde = "ITtilde"; //$NON-NLS-1$
  public final static String ITdarrow = "ITdarrow"; //$NON-NLS-1$
  public final static String ITminus = "ITminus"; //$NON-NLS-1$
  public final static String ITbang = "ITbang"; //$NON-NLS-1$
  public final static String ITstar = "ITstar"; //$NON-NLS-1$
  public final static String ITdot = "ITdot"; //$NON-NLS-1$

  public final static String ITbiglam = "ITbiglam"; //$NON-NLS-1$

  public final static String ITocurly = "ITocurly"; //$NON-NLS-1$
  public final static String ITccurly = "ITccurly"; //$NON-NLS-1$
  public final static String ITocurlybar = "ITocurlybar"; //$NON-NLS-1$
  public final static String ITccurlybar = "ITccurlybar"; //$NON-NLS-1$
  public final static String ITvocurly = "ITvocurly"; //$NON-NLS-1$
  public final static String ITvccurly = "ITvccurly"; //$NON-NLS-1$
  public final static String ITobrack = "ITobrack"; //$NON-NLS-1$
  public final static String ITopabrack = "ITopabrack"; //$NON-NLS-1$
  public final static String ITcpabrack = "ITcpabrack"; //$NON-NLS-1$
  public final static String ITcbrack = "ITcbrack"; //$NON-NLS-1$
  public final static String IToparen = "IToparen"; //$NON-NLS-1$
  public final static String ITcparen = "ITcparen"; //$NON-NLS-1$
  public final static String IToubxparen = "IToubxparen"; //$NON-NLS-1$
  public final static String ITcubxparen = "ITcubxparen"; //$NON-NLS-1$
  public final static String ITsemi = "ITsemi"; //$NON-NLS-1$
  public final static String ITcomma = "ITcomma"; //$NON-NLS-1$
  public final static String ITunderscore = "ITunderscore"; //$NON-NLS-1$
  public final static String ITbackquote = "ITbackquote"; //$NON-NLS-1$

  public final static String ITvarid = "ITvarid"; //$NON-NLS-1$
  public final static String ITconid = "ITconid"; //$NON-NLS-1$
  public final static String ITvarsym = "ITvarsym"; //$NON-NLS-1$
  public final static String ITconsym = "ITconsym"; //$NON-NLS-1$
  public final static String ITqvarid = "ITqvarid"; //$NON-NLS-1$
  public final static String ITqconid = "ITqconid"; //$NON-NLS-1$
  public final static String ITqvarsym = "ITqvarsym"; //$NON-NLS-1$
  public final static String ITqconsym = "ITqconsym"; //$NON-NLS-1$
  public final static String ITprefixqvarsym = "ITprefixqvarsym"; //$NON-NLS-1$
  public final static String ITprefixqconsym = "ITprefixqconsym"; //$NON-NLS-1$

  public final static String ITdupipvarid = "ITdupipvarid"; //$NON-NLS-1$

  public final static String ITchar = "ITchar"; //$NON-NLS-1$
  public final static String ITstring = "ITstring"; //$NON-NLS-1$
  public final static String ITinteger = "ITinteger"; //$NON-NLS-1$
  public final static String ITrational = "ITrational"; //$NON-NLS-1$

  public final static String ITprimchar = "ITprimchar"; //$NON-NLS-1$
  public final static String ITprimstring = "ITprimstring"; //$NON-NLS-1$
  public final static String ITprimint = "ITprimint"; //$NON-NLS-1$
  public final static String ITprimword = "ITprimword"; //$NON-NLS-1$
  public final static String ITprimfloat = "ITprimfloatl"; //$NON-NLS-1$
  public final static String ITprimdouble = "ITprimdouble"; //$NON-NLS-1$

  // Template Haskell extension tokens
  public final static String ITopenExpQuote = "ITopenExpQuote"; //$NON-NLS-1$
  public final static String ITopenPatQuote = "ITopenPatQuote"; //$NON-NLS-1$
  public final static String ITopenDecQuote = "ITopenDecQuote"; //$NON-NLS-1$
  public final static String ITopenTypQuote = "ITopenTypQuote"; //$NON-NLS-1$
  public final static String ITcloseQuote = "ITcloseQuote"; //$NON-NLS-1$
  public final static String ITidEscape = "ITidEscape"; //$NON-NLS-1$
  public final static String ITparenEscape = "ITparenEscape"; //$NON-NLS-1$
  public final static String ITvarQuote = "ITvarQuote"; //$NON-NLS-1$
  public final static String ITtyQuote = "ITtyQuote"; //$NON-NLS-1$
  public final static String ITquasiQuote = "ITquasiQuote"; //$NON-NLS-1$

  // Arrow notation extension
  public final static String ITproc = "ITproc"; //$NON-NLS-1$
  public final static String ITrec = "ITrec"; //$NON-NLS-1$
  public final static String IToparenbar = "IToparenbar"; //$NON-NLS-1$
  public final static String ITcparenbar = "ITcparenbar"; //$NON-NLS-1$
  public final static String ITlarrowtail = "ITlarrowtail"; //$NON-NLS-1$
  public final static String ITrarrowtail = "ITrarrowtail"; //$NON-NLS-1$
  public final static String ITLarrowtail = "ITLarrowtail"; //$NON-NLS-1$
  public final static String ITRarrowtail = "ITRarrowtail"; //$NON-NLS-1$

  public final static String ITunknown = "ITunknown"; //$NON-NLS-1$
  public final static String ITeof  = "ITeof"; //$NON-NLS-1$

  // Documentation annotations
  public final static String ITdocCommentNext = "ITdocCommentNext"; //$NON-NLS-1$
  public final static String ITdocCommentPrev = "ITdocCommentPrev"; //$NON-NLS-1$
  public final static String ITdocCommentNamed = "ITdocCommentNamed"; //$NON-NLS-1$
  public final static String ITdocSection = "ITdocSection"; //$NON-NLS-1$
  public final static String ITdocOptions = "ITdocOptions"; //$NON-NLS-1$
  public final static String ITdocOptionsOld = "ITdocOptionsOld"; //$NON-NLS-1$
  public final static String ITlineComment = "ITlineComment"; //$NON-NLS-1$
  public final static String ITblockComment = "ITblockComment"; //$NON-NLS-1$

  //=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~
  // Predicates
  //=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~

  /** 'import' token predicate
   *
   * @param token The lexer token to compare
   */
  public final static boolean isImportToken(final String token) {
    return ITimport.equals(token);
  }

  /** '::' (dcolon) token predicate
   *
   * @param token The lexer token to compare
   */
  public final static boolean isDoubleColon(final String token) {
    return ITdcolon.equals(token);
  }

  /**
   * '->' (rarrow) token predicate
   *
   * @param token The lexer token to compate
   */
  public final static boolean isRightArrow(final String token) {
    return ITrarrow.equals( token );
  }
}
