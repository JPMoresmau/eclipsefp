// Copyright (c) 2006 by Leif Frenzel <himself@leiffrenzel.de>
// All rights reserved.
package net.sf.eclipsefp.haskell.cabal.ui.internal.editors;

import org.eclipse.ui.editors.text.TextEditor;

/** <p>an editor for Cabal package description files.</p> 
  * 
  * <p>Note: this class is declared in <code>plugin.xml</code>.</p>
  *
  * @author Leif Frenzel
  */
public class CabalEditor extends TextEditor {

  public CabalEditor() {
    setSourceViewerConfiguration( new CabalConfiguration() );
  }
}
