// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.editor.text;

import net.sf.eclipsefp.haskell.ui.editor.IPartitionTypes;
import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.Token;

/** <p>A PartitionScanner divides up a document into non-overlapping
  * partitions, e.g. code parts, comments etc. Most editor functionalities
  * rely on partitioning.</p>
  *
  * @author Leif Frenzel
  */
public class HaskellPartitionScanner extends RuleBasedPartitionScanner {
  public HaskellPartitionScanner() {
    IPredicateRule[] rules = new IPredicateRule[] {
      // rule for multi line comments
      new MultiLineRule( "{-", "-}", new Token( IPartitionTypes.HS_COMMENT ) ),
      // rule for single line comments
      new EndOfLineRule( "--", new Token( IPartitionTypes.HS_COMMENT ) )
    };
    setPredicateRules( rules );
  }
}