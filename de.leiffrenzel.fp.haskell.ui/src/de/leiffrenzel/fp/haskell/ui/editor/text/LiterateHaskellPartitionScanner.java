// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.text;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.*;

import de.leiffrenzel.fp.haskell.ui.editor.IPartitionTypes;

/** <p>Partitions a Haskell document as literate Haskell (i.e. as text file
  * with embedded Haskell code as opposed to a source code file with 
  * embedded comments.</p>   
  * 
  * @author Leif Frenzel
  */
public class LiterateHaskellPartitionScanner extends RuleBasedPartitionScanner {

  public LiterateHaskellPartitionScanner() {
    IPredicateRule[] rules = new IPredicateRule[] {
      // rule for single line comments
      new EndOfLineRule( ">", new Token( IDocument.DEFAULT_CONTENT_TYPE ) )
    };
    setPredicateRules( rules );
    setDefaultReturnToken( new Token( IPartitionTypes.HS_LITERATE_COMMENT ) );
  }
}