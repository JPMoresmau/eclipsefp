// Copyright (c) 2003-2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;


/** <p>a degenerated rule based scanner that returns always the comment (or
  * literate comment) token.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
@Deprecated
public class HaskellCommentScanner extends RuleBasedScanner
                                   implements IEditorPreferenceNames {
  private final ScannerManager man;

  public HaskellCommentScanner( final ScannerManager man,final boolean literate ) {
    this.man=man;
    IToken defaultToken = ( literate ) ? createLiterateCommentToken()
                                       : createCommentToken();
    setDefaultReturnToken( defaultToken );
  }

  private IToken createCommentToken() {
    return man.createToken( EDITOR_COMMENT_COLOR,
                                                     EDITOR_COMMENT_BOLD );
  }

  private IToken createLiterateCommentToken() {
    return man.createToken( EDITOR_LITERATE_COMMENT_COLOR,
                            EDITOR_LITERATE_COMMENT_BOLD );
  }
}