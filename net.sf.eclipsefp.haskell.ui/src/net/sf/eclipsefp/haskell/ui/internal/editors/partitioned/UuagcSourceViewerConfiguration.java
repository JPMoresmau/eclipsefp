package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import net.sf.eclipsefp.haskell.core.codeassist.IScionTokens;
import net.sf.eclipsefp.haskell.scion.client.ScionInstance;
import net.sf.eclipsefp.haskell.scion.client.ScionPlugin;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.PartitionedScionTokenScanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Assert;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.PatternRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.WordPatternRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.text.source.ISourceViewer;


public class UuagcSourceViewerConfiguration extends
    PartitionSourceViewerConfiguration {

  /**
   * The constructor
   *
   * @param editor
   *          The associated Haskell editor
   */
  public UuagcSourceViewerConfiguration( final PartitionEditor editor ) {
    super( editor );
  }

  @Override
  public IPresentationReconciler getPresentationReconciler(
      final ISourceViewer viewer ) {
    PresentationReconciler reconciler = new PresentationReconciler();
    reconciler.setDocumentPartitioning( PartitionDocumentSetup.PARTITIONING );

    ScionInstance instance = null;
    if( editor != null ) {
      // Get the shared scion-server instance for lexing.
      instance = ScionPlugin.getSharedScionInstance();
      Assert.isNotNull( instance );
    } // else no editor: we're in preview null instance is fine


    IFile file = ( editor != null ? editor.findFile() : null );
    ITokenScanner codeScanner = new PartitionedScionTokenScanner(
        getScannerManager(), instance, file );
    DefaultDamagerRepairer haskellDr = new DefaultDamagerRepairer( codeScanner );
    reconciler.setDamager( haskellDr, PartitionDocumentSetup.HASKELL );
    reconciler.setRepairer( haskellDr, PartitionDocumentSetup.HASKELL );

    DefaultDamagerRepairer alexDr = new DefaultDamagerRepairer(
        createAlexScanner() );
    reconciler.setDamager( alexDr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( alexDr, IDocument.DEFAULT_CONTENT_TYPE );

    return reconciler;
  }

  private ITokenScanner createAlexScanner() {
    RuleBasedScanner scanner = new RuleBasedScanner();
    // Patterns
    WordPatternRule dollarVars = new WordPatternRule(
        KeywordDetector.NO_DIGIT_AT_START_DETECTOR, "$", "",
        tokenByTypes.get( IScionTokens.PREPROCESSOR_TEXT ) );
    WordPatternRule atVars = new WordPatternRule(
        KeywordDetector.NO_DIGIT_AT_START_DETECTOR, "@", "",
        tokenByTypes.get( IScionTokens.PREPROCESSOR_TEXT ) );
    PatternRule startCodes = new PatternRule( "<", ">",
        tokenByTypes.get( IScionTokens.IDENTIFIER_CONSTRUCTOR ), '\\', true );
    PatternRule regexSet = new PatternRule( "[", "]",
        tokenByTypes.get( IScionTokens.LITERAL_CHAR ), '\\', true );
    PatternRule string = new PatternRule( "\"", "\"",
        tokenByTypes.get( IScionTokens.LITERAL_STRING ), '\\', true );
    EndOfLineRule comment = new EndOfLineRule( "-- ",
        tokenByTypes.get( IScionTokens.LITERATE_COMMENT ) );
    // Single words
    WordRule colon = createRuleForToken( ";", IScionTokens.SYMBOL_RESERVED );
    WordRule pre = createRuleForToken( "^", IScionTokens.SYMBOL_RESERVED );
    WordRule post = createRuleForToken( "/", IScionTokens.SYMBOL_RESERVED );
    WordRule empty = createRuleForToken( "$", IScionTokens.SYMBOL_RESERVED );
    WordRule startRules = createRuleForToken( ":-",
        IScionTokens.SYMBOL_RESERVED );
    WordRule equals = createRuleForToken( "=", IScionTokens.SYMBOL_RESERVED );
    WordRule pipe = createRuleForToken( "|", IScionTokens.SYMBOL_RESERVED );
    WordRule wrapper = createRuleForToken( "%wrapper", IScionTokens.KEYWORD );

    scanner.setRules( new IRule[] { dollarVars, atVars, startCodes, regexSet,
        string, comment, colon, pre, post, empty, startRules, equals, pipe,
        wrapper } );
    return scanner;
  }
}
