// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.editors.haskell.IPartitionTypes;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;

/** <p>A PartitionScanner divides up a document into non-overlapping
  * partitions, e.g. code parts, comments etc. Most editor functionalities
  * rely on partitioning.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
public class HaskellPartitionScanner extends RuleBasedPartitionScanner {
  public HaskellPartitionScanner() {
    IPredicateRule[] rules = new IPredicateRule[] {
      // rule for single line comments
      new EndOfLineRule( "--", new Token( IPartitionTypes.HS_COMMENT ) ), //$NON-NLS-1$
      // rule for strings
      new SingleLineRule( "\"", //$NON-NLS-1$
                          "\"", //$NON-NLS-1$
                          new Token( IPartitionTypes.HS_STRING ),
                          '\\' ),
      // rule for character literals
      new SingleLineRule( "'", //$NON-NLS-1$
                          "'", //$NON-NLS-1$
                         new Token( IPartitionTypes.HS_CHARACTER ),
                          '\\' ),
      // rule for multi line comments
      new MultiLineRule( "{-", "-}", new Token( IPartitionTypes.HS_COMMENT ) ), //$NON-NLS-1$ //$NON-NLS-2$
    };
    setPredicateRules( rules );
  }
}