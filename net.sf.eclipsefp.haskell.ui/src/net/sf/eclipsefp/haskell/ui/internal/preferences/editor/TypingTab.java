// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.dialog.DialogField;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/** <p>Tab for the preference setting related to typing.</p>
  *
  * @author Leif Frenzel
  */
class TypingTab extends EditorTab implements IEditorPreferenceNames {
  private DialogField spaceForTabs;

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
    spaceForTabs=createBooleanField( composite, "Ins&ert space for tabs", prefName );
    initializeFields();

    return composite;
  }

  public void propertyChange( final PropertyChangeEvent event ) {
    if (spaceForTabs!=null){
      spaceForTabs.setInfo( getFromStore( IEditorPreferenceNames.EDITOR_SPACES_FOR_TABS ) );
    }

  }
}