package net.sf.eclipsefp.haskell.core.codeassist;

/**
 * Scion tokens: These are the token strings returned by scion to annotate various parts of the
 * input source, i.e., identify keywords, etc.
 *
 * @author B. Scott Michel (bscottm@ieee.org)
 */
public interface IScionTokens {
  // !!These strings need to be synchronized with changes to the scion source!! (See lib/Scion/Inspect.hs)
  public final static String LITERAL_STRING = "LS"; //$NON-NLS-1$
  public final static String LITERAL_CHAR = "LC"; //$NON-NLS-1$
  public final static String DOCUMENTATION_ANNOTATION = "D"; //$NON-NLS-1$
  public final static String LITERATE_COMMENT = "DL"; //$NON-NLS-1$
  public final static String KEYWORD = "K"; //$NON-NLS-1$
  public final static String GHC_EXTENSION_KEYWORD = "EK"; //$NON-NLS-1$
  public final static String LITERAL_INTEGER = "LI"; //$NON-NLS-1$
  public final static String LITERAL_RATIONAL = "LR"; //$NON-NLS-1$
  public final static String LITERAL_WORD = "LW"; //$NON-NLS-1$
  public final static String LITERAL_FLOAT = "LF"; //$NON-NLS-1$
  public final static String IDENTIFIER_CONSTRUCTOR = "IC"; // aka "conid" //$NON-NLS-1$
  public final static String IDENTIFIER_VARIABLE = "IV"; // aka "varid" //$NON-NLS-1$
  public final static String SYMBOL_RESERVED = "S"; //$NON-NLS-1$
  public final static String SYMBOL_SPECIAL = "SS"; //$NON-NLS-1$
  public final static String PREPROCESSOR_TEXT = "PP"; //$NON-NLS-1$
  public final static String TEMPLATE_HASKELL = "TH"; //$NON-NLS-1$
}
