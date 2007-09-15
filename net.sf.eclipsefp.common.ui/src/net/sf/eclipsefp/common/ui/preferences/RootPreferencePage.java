// Copyright (c) 2003-2004 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.common.ui.preferences;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


/** <p>A convenience class for preference pages which are at the root of 
  * a tree and provide no editors but display only a message.</p>
  * 
  * @author Leif Frenzel
  */
public abstract class RootPreferencePage extends FieldEditorPreferencePage
                                         implements IWorkbenchPreferencePage {

  public RootPreferencePage( final String message ) {
    super( GRID );
    setDescription( message );
    noDefaultAndApplyButton();
  }
  
  @Override
  public void createFieldEditors() {
    // empty implementation
  }
  
  public void init( final IWorkbench workbench ) {
    // empty implementation
  }
}