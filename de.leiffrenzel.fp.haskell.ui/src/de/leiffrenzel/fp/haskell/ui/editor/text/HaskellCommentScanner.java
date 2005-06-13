// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package de.leiffrenzel.fp.haskell.ui.editor.text;

import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;

import de.leiffrenzel.fp.haskell.ui.preferences.editor.IEditorPreferenceNames;


/** <p>a degenerated rule based scanner that returns always the comment (or
  * literate comment) token.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellCommentScanner extends RuleBasedScanner 
                                   implements IEditorPreferenceNames {

  public HaskellCommentScanner( final boolean literate ) {
    IToken defaultToken = ( literate ) ? createLiterateCommentToken() 
                                       : createCommentToken();
    setDefaultReturnToken( defaultToken );
  }

  private IToken createCommentToken() {
    return ScannerManager.getInstance().createToken( EDITOR_COMMENT_COLOR, 
                                                     EDITOR_COMMENT_BOLD );
  }
  
  private IToken createLiterateCommentToken() {
    ScannerManager man = ScannerManager.getInstance();
    return man.createToken( EDITOR_LITERATE_COMMENT_COLOR, 
                            EDITOR_LITERATE_COMMENT_BOLD );
  }
}