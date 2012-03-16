/**
 * (c) 2011, Alejandro Serrano
 * Released under the terms of the EPL.
 */
package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import java.util.ArrayList;
import net.sf.eclipsefp.haskell.core.codeassist.IScionTokens;
import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text.PartitionedScionTokenScanner;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.ICharacterScanner;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.ITokenScanner;
import org.eclipse.jface.text.rules.PatternRule;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WordPatternRule;
import org.eclipse.jface.text.source.ISourceViewer;

/**
 * Configuration for highlighting of UUAGC files.
 * @author Alejandro Serrano
 *
 */
public class UuagcSourceViewerConfiguration extends
    PartitionSourceViewerConfiguration {

  private static String[] symbols = new String[] { "[", "|", "]", "=>", "=",
      "::", ":" };
  private static String[] keywords = new String[] { "data", "attr", "sem",
      "deriving", "use", "type", "include", "ext", "merge", "as", "uniqueref",
      "around", "pragma", "module", "attach" };
  private static String[] onlySmallKeywords = new String[] { "inh", "syn" };
  private static String[] specialVariables = new String[] { "lhs", "loc" };
  private static String[] types = new String[] { "Maybe", "Either", "Map",
      "IntMap", "self" };


  public UuagcSourceViewerConfiguration( final PartitionEditor editor ) {
    super( editor );
  }

  @Override
  public IPresentationReconciler getPresentationReconciler(
      final ISourceViewer viewer ) {
    PresentationReconciler reconciler = new PresentationReconciler();
    reconciler.setDocumentPartitioning( PartitionDocumentSetup.PARTITIONING );


    IFile file = ( editor != null ? editor.findFile() : null );
    ITokenScanner codeScanner = new PartitionedScionTokenScanner(
        getScannerManager(), file, new String[] { "{" },
        new String[] { "}" }, new String[] { "{-" }, new String[] { "-}" } );
    DefaultDamagerRepairer haskellDr = new DefaultDamagerRepairer( codeScanner );
    reconciler.setDamager( haskellDr, PartitionDocumentSetup.HASKELL );
    reconciler.setRepairer( haskellDr, PartitionDocumentSetup.HASKELL );

    DefaultDamagerRepairer uuagcDr = new DefaultDamagerRepairer(
        createUuagcScanner() );
    reconciler.setDamager( uuagcDr, IDocument.DEFAULT_CONTENT_TYPE );
    reconciler.setRepairer( uuagcDr, IDocument.DEFAULT_CONTENT_TYPE );

    return reconciler;
  }

  private ITokenScanner createUuagcScanner() {
    RuleBasedScanner scanner = new RuleBasedScanner();
    // Patterns
    WordPatternRule vars = new WordPatternRule(
        KeywordDetector.NO_DIGIT_AT_START_DETECTOR, "@", "",
        tokenByTypes.get( IScionTokens.PREPROCESSOR_TEXT ) );
    PatternRule string = new PatternRule( "\"", "\"",
        tokenByTypes.get( IScionTokens.LITERAL_STRING ), '\\', true );
    EndOfLineRule comment = new EndOfLineRule( "-- ",
        tokenByTypes.get( IScionTokens.LITERATE_COMMENT ) );


    ArrayList<IRule> rules = new ArrayList<IRule>();
    rules.add( vars );
    rules.add( string );
    rules.add( comment );

    for( String symbol: symbols ) {
      rules.add( createRuleForToken( symbol, IScionTokens.SYMBOL_RESERVED ) );
    }
    for( String keyword: keywords ) {
      rules.add( createRuleForToken( keyword, IScionTokens.KEYWORD ) );
      rules.add( createRuleForToken( keyword.toUpperCase(),
          IScionTokens.KEYWORD ) );
    }
    for( String keyword: onlySmallKeywords ) {
      rules.add( createRuleForToken( keyword, IScionTokens.KEYWORD ) );
    }
    for( String var: specialVariables ) {
      rules.add( createRuleForToken( var, IScionTokens.PREPROCESSOR_TEXT ) );
    }
    for( String type: types ) {
      rules
          .add( createRuleForToken( type, IScionTokens.IDENTIFIER_CONSTRUCTOR ) );
      rules.add( createRuleForToken( type.toUpperCase(),
          IScionTokens.IDENTIFIER_CONSTRUCTOR ) );
    }

    IRule names = new IRule() {

      @Override
      public IToken evaluate( final ICharacterScanner scanner ) {
        // First we need an uppercase letter
        int firstLetter = scanner.read();
        if (firstLetter == ICharacterScanner.EOF || !Character.isUpperCase( firstLetter )) {
          scanner.unread();
          return Token.UNDEFINED;
        }

        // If this was OK, continue until we arrive to a whitespace
        while (true) {
          int nextLetter = scanner.read();
          if (!Character.isJavaIdentifierPart( nextLetter )) {
            scanner.unread();
            return tokenByTypes.get( IScionTokens.IDENTIFIER_CONSTRUCTOR );
          }
        }
      }
    };
    rules.add(names);

    scanner.setRules( rules.toArray( new IRule[ rules.size() ] ) );

    return scanner;
  }
}
