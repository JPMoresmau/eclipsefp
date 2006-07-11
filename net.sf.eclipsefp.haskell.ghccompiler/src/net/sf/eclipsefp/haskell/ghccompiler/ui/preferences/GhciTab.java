// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;

/** <p>the single tab on the GHCi preference page (not shown as tab, 
  * but uses the preference access mechanism of tabs).</p>
  * 
  * @author Leif Frenzel
  */
class GhciTab extends Tab implements IGhcPreferenceNames {
  
  GhciTab( final IPreferenceStore store ) {
    super( store );
  }
  
  
  // interface methods of Tab
  ///////////////////////////
  
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );
    
    String text = "Use GHC compiler settings for GHCi";
    
    BooleanDialogField result = new BooleanDialogField( composite, text );
    result.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( GHCI_USES_GHC_OPTIONS, selected );
      }
    } );
    result.setInfo( getFromStore( GHCI_USES_GHC_OPTIONS ) );
    
    Label lblNote = new Label( composite, SWT.WRAP );
    String note =   "If you check this, please make sure that you have "
                  + "configured only options that make sense for GHCi.\n"
                  + "Refer to the GHC manual for more information.";
    lblNote.setText( note );
    
    return composite;
  }
  
  
  // helping methods
  //////////////////

  private Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }  
}