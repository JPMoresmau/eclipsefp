// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ui.internal.preferences.editor;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Composite;


/** <p>the superclass for all tabs on the editor preference page.
  * Encapsulates some common functionality.</p>
  *
  * @author Leif Frenzel
  * @deprecated
  */
abstract class EditorTab extends Tab {

  EditorTab( final IPreferenceStore store ) {
    super( store );
  }


  // functionality for subclasses
  ///////////////////////////////

  DialogField createBooleanField( final Composite parent,
                                  final String text,
                                  final String name ) {
    BooleanDialogField result = new BooleanDialogField( parent, text );
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

  Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }
}