// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.ui.internal.editors.cabal.text;

import net.sf.eclipsefp.haskell.ui.HaskellUIPlugin;
import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
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
    Color commentColor = new ColorProvider(HaskellUIPlugin.getDefault().getPreferenceStore()).getColor( IEditorPreferenceNames.EDITOR_COMMENT_COLOR );
    TextAttribute textAtt = new TextAttribute( commentColor );
    setDefaultReturnToken( new Token( textAtt ) );
  }
}
