// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.swt.graphics.Color;

/** <p>a trivial token scanner for comment partitions.</p>
  *
  * @author Leif Frenzel
  */
public class CommentScanner extends RuleBasedScanner {

  public CommentScanner() {
    Color commentColor = ColorProvider.getInstance().getColor( ColorProvider.COMMENT );
    TextAttribute textAtt = new TextAttribute( commentColor );
    setDefaultReturnToken( new Token( textAtt ) );
  }
}
