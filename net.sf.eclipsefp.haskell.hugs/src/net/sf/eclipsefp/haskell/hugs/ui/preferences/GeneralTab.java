// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.hugs.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.ExecutableDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.hugs.core.preferences.IHugsPreferenceNames;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;


/** <p>The tab on the HUGS preference page that displays general 
  * information.</p>
  * 
  * @author Leif Frenzel
  */
public class GeneralTab extends Tab implements IHugsPreferenceNames {
  
  public GeneralTab( final IPreferenceStore store ) {
    super( store );
  }

  // interface methods of Tab
  ///////////////////////////
  
  public Control createControl( final Composite parent ) {
    String labelText = "HUGS executable";
    ExecutableDialogField dlgField = new ExecutableDialogField( parent, 
                                                                labelText ) {
      protected String createDisplayContent( final String info ) {
        // TODO
        return "Executable check not implemented.";
      }
    };

    dlgField.setInfo( getPreferenceStore().getString( EXECUTABLE_NAME ) );
    
    dlgField.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        getPreferenceStore().setValue( EXECUTABLE_NAME, ( String )newInfo );
      }
    } );
    return dlgField;
  }
}