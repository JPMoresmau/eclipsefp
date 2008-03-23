// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.IPartitionTypes;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

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