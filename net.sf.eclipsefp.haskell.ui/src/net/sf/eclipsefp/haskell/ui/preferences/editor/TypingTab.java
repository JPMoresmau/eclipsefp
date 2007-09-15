// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.preferences.editor;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/** <p>Tab for the preference setting related to typing.</p>
  *
  * @author Leif Frenzel  
  */
class TypingTab extends EditorTab implements IEditorPreferenceNames {

  TypingTab( final IPreferenceStore store ) {
    super( store );
  }

  
  // interface methods of Tab
  ///////////////////////////
  
  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );

    GridLayout layout = new GridLayout();
    layout.numColumns = 2;
    composite.setLayout( layout );

    String prefName = IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS;
    createBooleanField( composite, "Ins&ert space for tabs", prefName );
    initializeFields();

    return composite;
  }
}