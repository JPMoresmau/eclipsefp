/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import net.sf.eclipsefp.haskell.core.codeassist.ITokenTypes;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.ScionTokenScanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.PatternRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.text.source.ISourceViewer;

/**
 * Configures hihlighting for Happy parsers.
 * @author Alejandro Serrano
 *
 */
public class HappySourceViewerConfiguration extends
    PartitionSourceViewerConfiguration {

  /**
   * The constructor
   *
   * @param editor
   *          The associated Haskell editor
   */
  public HappySourceViewerConfiguration( final PartitionEditor editor ) {
    super( editor );
  }

  @Override
  public IPresentationReconciler getPresentationReconciler(
      final ISourceViewer viewer ) {
    PresentationReconciler reconciler = new PresentationReconciler();
    reconciler.setDocumentPartitioning( PartitionDocumentSetup.PARTITIONING );


    IFile file = ( editor != null ? editor.findFile() : null );
//    ITokenScanner codeScanner = new PartitionedScionTokenScanner(
//        getScannerManager(), file, new String[] { "{%%", "{%^", "{%",
//            "{" }, new String[] { "}" }, new String[] { "{-" },
//        new String[] { "-}" } );
    ScionTokenScanner codeScanner = new ScionTokenScanner(
        getScannerManager(), file );
    DefaultDamagerRepairer haskellDr = new DefaultDamagerRepairer( codeScanner );
    reconciler.setDamager( haskellDr, PartitionDocumentSetup.HASKELL );
    reconciler.setRepairer( haskellDr, PartitionDocumentSetup.HASKELL );

    DefaultDamagerRepairer happyDr = new DefaultDamagerRepairer(
        createHappyScanner() );
    reconciler.setDamager( happyDr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( happyDr, IDocument.DEFAULT_CONTENT_TYPE );

    if (editor!=null){
      editor.setTokenScanner( codeScanner );
    }

    return reconciler;
  }

  private ITokenScanner createHappyScanner() {
    RuleBasedScanner scanner = new RuleBasedScanner();
    // Patterns
    PatternRule chars = new PatternRule( "'", "'",
        tokenByTypes.get( ITokenTypes.LITERAL_STRING ), '\\', true );
    PatternRule string = new PatternRule( "\"", "\"",
        tokenByTypes.get( ITokenTypes.LITERAL_STRING ), '\\', true );
    EndOfLineRule comment = new EndOfLineRule( "-- ",
        tokenByTypes.get( ITokenTypes.LITERATE_COMMENT ) );
    // Single words
    WordRule semicolon = createRuleForToken( ":",
        ITokenTypes.SYMBOL_RESERVED );
    WordRule pipe = createRuleForToken( "|",
        ITokenTypes.SYMBOL_RESERVED );
    WordRule doublecolon = createRuleForToken( "::",
        ITokenTypes.SYMBOL_RESERVED );
    WordRule doublepercent = createRuleForToken( "%%",
        ITokenTypes.SYMBOL_RESERVED );
    // Keywords
    WordRule name = createRuleForToken( "%name", ITokenTypes.KEYWORD );
    WordRule error = createRuleForToken( "%error", ITokenTypes.KEYWORD );
    WordRule token = createRuleForToken( "%token", ITokenTypes.KEYWORD );
    WordRule tokentype = createRuleForToken( "%tokentype", ITokenTypes.KEYWORD );
    WordRule right = createRuleForToken( "%right", ITokenTypes.KEYWORD );
    WordRule left = createRuleForToken( "%left", ITokenTypes.KEYWORD );
    WordRule nonassoc = createRuleForToken( "%nonassoc", ITokenTypes.KEYWORD );
    WordRule prec = createRuleForToken( "%prec", ITokenTypes.KEYWORD );
    WordRule monad = createRuleForToken( "%monad", ITokenTypes.KEYWORD );
    WordRule lexer = createRuleForToken( "%lexer", ITokenTypes.KEYWORD );
    WordRule attribute = createRuleForToken( "%attribute", ITokenTypes.KEYWORD );
    WordRule attributetype = createRuleForToken( "%attributetype",
        ITokenTypes.KEYWORD );
    WordRule partial = createRuleForToken( "%partial", ITokenTypes.KEYWORD );
    WordRule expect = createRuleForToken( "%expect", ITokenTypes.KEYWORD );

    scanner
        .setRules( new IRule[] { chars, string, comment, semicolon, pipe,
            doublecolon, doublepercent, name, error, token, tokentype, right,
            left, nonassoc, prec, monad, lexer, attribute, attributetype,
            partial, expect } );
    return scanner;
  }
}
