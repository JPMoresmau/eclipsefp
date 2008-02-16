// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences;

import org.eclipse.jface.preference.PreferencePage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;


/** <p>the preference page for the Haskell interpreters.</p>
  * 
  * @author Leif Frenzel
  */
public class HaskellInterpreterPP extends PreferencePage 
                                  implements IWorkbenchPreferencePage,
                                             IPreferenceConstants {


  // interface methods of PreferencePage
  //////////////////////////////////////
  
  @Override
  protected Control createContents( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout() );
    
    Label label = new Label( composite, SWT.NONE );
    String text =   "Haskell interpreters have added their preference pages\n"
                  + "under this category.";
    label.setText( text );
    
    return composite;
  }

  
  // interface methods of IWorkbenchPreferencePage
  ////////////////////////////////////////////////
  
  public void init( final IWorkbench workbench ) {
    // unused
  }
}