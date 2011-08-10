package net.sf.eclipsefp.haskell.ui.internal.editors.partitioned;

import org.eclipse.core.filebuffers.IDocumentSetupParticipant;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IDocumentExtension3;
import org.eclipse.jface.text.IDocumentPartitioner;
import org.eclipse.jface.text.rules.FastPartitioner;
import org.eclipse.jface.text.rules.IPartitionTokenScanner;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.PatternRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;


public class HappyDocumentSetup implements IDocumentSetupParticipant {

  public static final String PARTITIONING = "org.eclipse.editor";
  public static final String HASKELL = "net.sf.eclipsefp.haskell.ui.internal.editors.haskell.HaskellEditor";
  public static final String[] TYPES = new String[] {
      IDocument.DEFAULT_CONTENT_TYPE, HASKELL };

  public void setup( final IDocument document ) {
    IDocumentPartitioner p = new FastPartitioner(
        createHaskellPartitionScanner(), TYPES );
    ( ( IDocumentExtension3 )document )
        .setDocumentPartitioner( PARTITIONING, p );
    p.connect( document );
  }

  private IPartitionTokenScanner createHaskellPartitionScanner() {
    RuleBasedPartitionScanner scanner = new RuleBasedPartitionScanner();
    scanner.setPredicateRules( new IPredicateRule[] { new PatternRule( "{",
        "}", new Token( HASKELL ), '\\', false ) } );
    return scanner;
  }

}
