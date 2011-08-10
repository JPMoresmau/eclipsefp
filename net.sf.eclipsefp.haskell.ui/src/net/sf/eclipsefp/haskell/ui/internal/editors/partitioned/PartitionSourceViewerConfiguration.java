package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;


import java.util.HashMap;
import java.util.Map;
import net.sf.eclipsefp.haskell.core.codeassist.IScionTokens;
import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScannerManager;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;

public class PartitionSourceViewerConfiguration extends
    SourceViewerConfiguration implements IEditorPreferenceNames {

  /** The associated editor */
  protected final PartitionEditor editor;
  /** The plugin's preference store */
  protected IPreferenceStore prefStore;
  /** The syntax highlighting and other content management container */
  protected ScannerManager scannerManager;

  protected final Map<String, IToken> tokenByTypes;

  public PartitionSourceViewerConfiguration( final PartitionEditor editor ) {
    super();
    this.editor = editor;

    this.tokenByTypes = new HashMap<String, IToken>() {

      // Eclipse insists on a serial version identifier, not that this hash map
      // will ever
      // get serialized...
      private static final long serialVersionUID = 3579246300065591883L;
      {
        put(
            IScionTokens.LITERAL_STRING,
            getScannerManager().createToken( EDITOR_STRING_COLOR,
                EDITOR_STRING_BOLD ) );
        put(
            IScionTokens.LITERAL_CHAR,
            getScannerManager().createToken( EDITOR_CHAR_COLOR,
                EDITOR_CHAR_BOLD ) );
        put( IScionTokens.DOCUMENTATION_ANNOTATION, getScannerManager()
            .createToken( EDITOR_COMMENT_COLOR, EDITOR_COMMENT_BOLD ) );
        put(
            IScionTokens.LITERATE_COMMENT,
            getScannerManager().createToken( EDITOR_LITERATE_COMMENT_COLOR,
                EDITOR_LITERATE_COMMENT_BOLD ) );
        put(
            IScionTokens.KEYWORD,
            getScannerManager().createToken( EDITOR_KEYWORD_COLOR,
                EDITOR_KEYWORD_BOLD ) );
        put( IScionTokens.GHC_EXTENSION_KEYWORD, getScannerManager()
            .createToken( EDITOR_KEYWORD_COLOR, EDITOR_KEYWORD_BOLD ) );
        put(
            IScionTokens.LITERAL_INTEGER,
            getScannerManager().createToken( EDITOR_NUMBER_COLOR,
                EDITOR_NUMBER_BOLD ) );
        put(
            IScionTokens.LITERAL_RATIONAL,
            getScannerManager().createToken( EDITOR_NUMBER_COLOR,
                EDITOR_NUMBER_BOLD ) );
        put(
            IScionTokens.LITERAL_WORD,
            getScannerManager().createToken( EDITOR_NUMBER_COLOR,
                EDITOR_NUMBER_BOLD ) );
        put(
            IScionTokens.LITERAL_FLOAT,
            getScannerManager().createToken( EDITOR_NUMBER_COLOR,
                EDITOR_NUMBER_BOLD ) );
        put( IScionTokens.IDENTIFIER_CONSTRUCTOR, getScannerManager()
            .createToken( EDITOR_CON_COLOR, EDITOR_CON_BOLD ) );
        put( IScionTokens.IDENTIFIER_VARIABLE,
            getScannerManager().createToken( EDITOR_VAR_COLOR, EDITOR_VAR_BOLD ) );
        put(
            IScionTokens.SYMBOL_RESERVED,
            getScannerManager().createToken( EDITOR_SYMBOL_COLOR,
                EDITOR_SYMBOL_BOLD ) );
        put(
            IScionTokens.SYMBOL_SPECIAL,
            getScannerManager().createToken( EDITOR_SYMBOL_COLOR,
                EDITOR_SYMBOL_BOLD ) );
        put( IScionTokens.PREPROCESSOR_TEXT,
            getScannerManager().createToken( EDITOR_CPP_COLOR, EDITOR_CPP_BOLD ) );
        put( IScionTokens.TEMPLATE_HASKELL,
            getScannerManager().createToken( EDITOR_TH_COLOR, EDITOR_TH_BOLD ) );
      }
    };
  }

  protected WordRule createRuleForToken( final String string, final String token ) {
    WordRule rule = new WordRule( new SingleWordDetector( string ) );
    rule.addWord( string, tokenByTypes.get( token ) );
    return rule;
  }

  @Override
  public int getTabWidth( final ISourceViewer sourceViewer ) {
    return getPreferenceStore().getInt( EDITOR_TAB_WIDTH );
  }

  protected IPreferenceStore getPreferenceStore() {
    if( prefStore != null ) {
      return prefStore;
    }
    return HaskellUIPlugin.getDefault().getPreferenceStore();
  }

  public void setPreferenceStore( final IPreferenceStore prefStore ) {
    this.prefStore = prefStore;
  }

  /**
   * Get the scanner manager. If the preference store (prefStore) is set, then
   * return a new {@link ScannerManager} that uses the preference store;
   * otherwise, return the ScannerManager singleton instance.
   * */
  public ScannerManager getScannerManager() {
    if( prefStore != null ) {
      if( scannerManager == null ) {
        scannerManager = new ScannerManager( prefStore );
      }
      return scannerManager;
    }
    return ScannerManager.getInstance();
  }

}