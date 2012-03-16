// Copyright (c) 2003-2005 by Leif Frenzel - see http://leiffrenzel.de
package net.sf.eclipsefp.haskell.ghccompiler.ui.preferences;

import net.sf.eclipsefp.common.ui.dialog.BooleanDialogField;
import net.sf.eclipsefp.common.ui.dialog.DialogField;
import net.sf.eclipsefp.common.ui.dialog.IDialogFieldListener;
import net.sf.eclipsefp.common.ui.preferences.Tab;
import net.sf.eclipsefp.haskell.core.compiler.CompilerManager;
import net.sf.eclipsefp.haskell.core.compiler.HsImplementationType;
import net.sf.eclipsefp.haskell.core.compiler.IHsImplementation;
import net.sf.eclipsefp.haskell.ghccompiler.core.GhcParameter;
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
    String text = ParamsUITexts.getShortDescription( name );
    String tooltip = text + "\n" + name; //$NON-NLS-1$
    BooleanDialogField result = new BooleanDialogField( parent, text, tooltip );
    result.addDialogFieldListener( new IDialogFieldListener() {
      @Override
      public void infoChanged( final Object newInfo ) {
        boolean selected = ( ( Boolean )newInfo ).booleanValue();
        getPreferenceStore().setValue( name, selected );
      }
    } );
    result.setInfo( getFromStore( name ) );
    return result;
  }

  DialogField createBooleanField( final Composite parent, final GhcParameter p ) {
    final String name=p.getName();
    String displayName=name;
    IHsImplementation impl=CompilerManager.getInstance().getCurrentHsImplementation();
    if (impl!=null && impl.getType().equals( HsImplementationType.GHC )){
      displayName=p.getName( impl.getVersion() );
    }
    String text = ParamsUITexts.getShortDescription( name );
    String tooltip = text + "\n" + displayName; //$NON-NLS-1$
    BooleanDialogField result = new BooleanDialogField( parent, text, tooltip );
    result.addDialogFieldListener( new IDialogFieldListener() {
      @Override
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

  protected Boolean getFromStore( final String name ) {
    boolean value = getPreferenceStore().getBoolean( name );
    return ( value ) ? Boolean.TRUE : Boolean.FALSE;
  }
}