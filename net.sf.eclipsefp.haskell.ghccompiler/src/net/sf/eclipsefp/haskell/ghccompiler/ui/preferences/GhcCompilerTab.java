// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;




/** <p>superclass for all tabs on the GHC compiler preference page,
  * encapsulates some common functionality.</p>
  *
  * @author Leif Frenzel
  */
public abstract class GhcCompilerTab extends Tab {

  public GhcCompilerTab( final IPreferenceStore store ) {
    super( store );
  }


  // functionality for subclasses
  ///////////////////////////////

  DialogField createBooleanField( final Composite parent, final String name ) {
    String text = UITexts.getShortDescription( name );
    String tooltip = text + "\n" + name; //$NON-NLS-1$
    BooleanDialogField result = new BooleanDialogField( parent, text, tooltip );
    result.addDialogFieldListener( new IDialogFieldListener() {
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( name, selected );
      }
    } );
    result.setInfo( getFromStore( name ) );
    return result;
  }


  // helping methods
  //////////////////

  private Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }
}