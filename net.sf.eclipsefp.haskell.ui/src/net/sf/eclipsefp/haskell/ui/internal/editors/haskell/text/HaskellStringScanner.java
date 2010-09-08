// Copyright (c) 2008 by Leif Frenzel - see http://leiffrenzel.de
// This code is made available under the terms of the Eclipse Public License,
// version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
package net.sf.eclipsefp.haskell.ui.internal.editors.haskell.text;

import net.sf.eclipsefp.haskell.ui.internal.preferences.editor.IEditorPreferenceNames;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;


/** <p>a degenerated rule based scanner that returns always the string
  *  literal token.</p>
  *
  * @author Leif Frenzel
  */
@Deprecated
public class HaskellStringScanner extends RuleBasedScanner
                                  implements IEditorPreferenceNames {

  public HaskellStringScanner(final ScannerManager man) {
    IToken token = man.createToken( EDITOR_STRING_COLOR, EDITOR_STRING_BOLD );
    setDefaultReturnToken( token );
  }
}