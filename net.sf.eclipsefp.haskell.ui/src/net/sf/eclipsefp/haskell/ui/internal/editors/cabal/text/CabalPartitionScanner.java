// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import net.sf.eclipsefp.haskell.ui.internal.editors.cabal.CabalDocProvider;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

/** <p>the partition scanner for cabal files.</p>
  *
  * @author Leif Frenzel
  */
public class CabalPartitionScanner extends RuleBasedPartitionScanner {

  public CabalPartitionScanner() {
    IPredicateRule[] rules = new IPredicateRule[] {
      // rule for single line comments
      new EndOfLineRule( "--",  //$NON-NLS-1$
    		             new Token( CabalDocProvider.COMMENT_CONTENT_TYPE ) )
    };
    setPredicateRules( rules );
  }
}
