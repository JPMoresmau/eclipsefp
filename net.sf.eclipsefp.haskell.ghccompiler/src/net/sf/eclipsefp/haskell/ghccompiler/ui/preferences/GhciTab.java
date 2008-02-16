// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.ghccompiler.core.preferences.IGhcPreferenceNames;
import net.sf.eclipsefp.haskell.ghccompiler.ui.internal.util.UITexts;
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

  @Override
  public Control createControl( final Composite parent ) {
    Composite composite = new Composite( parent, SWT.NONE );
    composite.setLayout( new GridLayout( 1, false ) );

    String text = UITexts.ghciTab_options;
    BooleanDialogField result = new BooleanDialogField( composite, text );
    result.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( GHCI_USES_GHC_OPTIONS, selected );
      }
    } );
    result.setInfo( getFromStore( GHCI_USES_GHC_OPTIONS ) );

    Label lblNote = new Label( composite, SWT.WRAP );
    lblNote.setText( UITexts.ghciTab_note );

    return composite;
  }


  // helping methods
  //////////////////

  private Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }
}